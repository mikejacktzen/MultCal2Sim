library(dplyr)
library(survey)
library(sampling)

library(doParallel)
# detectCores()
registerDoParallel(cores=1)
# registerDoParallel(cores=32)

# source("rprojects/synth_2_uncert/scripts//a_comp_targ_from_design_helper.R")
# source("rprojects/synth_2_uncert/scripts//b_0_combine_est.R")
# source("rprojects/synth_2_uncert/scripts//b_1_b_func_mps_mod.R")
# source("rprojects/synth_2_uncert/scripts//b_2_b_func_mrs_mod.R")

source("R/helpers.R")

form_outcome = ~enroll
form_poststrat = ~sch.wide


data(api,package = 'survey')
# ?api

# use a 1-imputed dataset as the population
library(mice)
# ?mice

dat_pop = mice::complete(mice::mice(apipop,m=1))

# dat_pop = readRDS("rprojects/synth_2_uncert/scripts//dat_pop.rds")


# stype and dnum had no missing
size_ref_strata = xtabs(~stype,apistrat) %>% as.numeric()
size_ref_cluster = apiclus1$dnum %>% unique() %>% length()

draw_samp_designs = function(data_pop,
                             size_ref_strata,
                             size_ref_cluster){

  # data_pop = dat_pop
  # summary(data_pop)
  library(sampling)

  # draw stratified sample

  # apistrat is a sample stratified by stype,
  # size_ref_strata = xtabs(~stype,apistrat) %>% as.numeric()

  # ?sampling::strata
  draw_strata = sampling::strata(data_pop,
                                 stratanames=c("stype"),
                                 size=size_ref_strata,
                                 method="srswor",
                                 description=FALSE)

  samp_strata = sampling::getdata(data_pop,draw_strata)
  # dim(data_pop)
  # dim(samp_strata)

  samp_strata$wt = 1/samp_strata$Prob

  des_strat = survey::svydesign(id=~ID_unit,
                                strata=~Stratum,
                                weights=~wt,
                                data=samp_strata)

  # draw cluster sample
  # ?apiclus1
  # apiclus1 is a cluster sample of school districts,
  # names(apiclus1)
  # apiclus1$dnum %>% unique() %>% length()

  draw_cluster = sampling::cluster(data=data_pop,
                                   clustername=c("dnum"),
                                   size=size_ref_cluster,
                                   method="srswor")

  # extracts the observed data
  # the order of the columns is different from the order in the initial database
  samp_cluster = sampling::getdata(data_pop, draw_cluster)
  samp_cluster$wt = 1/samp_cluster$Prob

  # design cluster
  des_clust <- survey::svydesign(id=~ID_unit,
                                 weights=~wt,
                                 data=samp_cluster)

  list_des_out = list(des_strat=des_strat,
                      des_clust=des_clust)
  return(list_des_out)
}


# MPS ---------------------------------------------------------------------

n_trial = 1

df_cov_mps = foreach(icount(n_trial),
                     .packages=c('survey'),
                     .combine=rbind) %dopar% {

                       list_samps = draw_samp_designs(data_pop=dat_pop,
                                                      size_ref_strata,
                                                      size_ref_cluster)

                       #   form_outcome = ~enroll
                       # form_poststrat = ~sch.wide

                       des_targ = list_samps$des_strat
                       des_2be_cal = list_samps$des_clust

                       df_targ_tot_joint = targ_tot_from_des_joint(formula_additive=form_poststrat,
                                                                    design_target=des_targ)

                       df_targ_tot_joint_asifc = subset(df_targ_tot_joint,select=-c(SE))
                       des_poststrat2_asifc = survey::postStratify(design = des_2be_cal,
                                                                   population=df_targ_tot_joint_asifc,
                                                                   partial=TRUE,
                                                                   strata=form_poststrat)

                       est_asifc = data.frame(svytotal(x=form_outcome,
                                                       design=des_poststrat2_asifc,
                                                       na.rm=TRUE))
                       names(est_asifc) = c('theta_asifc','se_asifc')

                       est_out_mps = sim_m_poststrats(m_target_sim=5,
                                                      design_2be_strat=des_2be_cal,
                                                      lgl_rej_neg_sim = FALSE,
                                                      df_targ_tot_joint=df_targ_tot_joint,
                                                      form_poststrat=form_poststrat,
                                                      form_outcome=form_outcome)

                       cat_1_mps = lapply(est_out_mps,function(x){x[1,]})
                       out_pool_cat_1_mps = do.call(rbind,cat_1_mps)

                       est_mps = combine_est(out_pool_cat_1_mps)[,c('theta_mcs','se_root_T')]
                       names(est_mps) = c('theta_mcs','se_mcs')
                       out_ests = data.frame(est_mps,est_asifc)

                     }

# dim(df_cov_mps)
# head(df_cov_mps)

val_pop = sum(apipop[labels(terms(form_outcome))],na.rm=TRUE)

df_out_mps = df_cov_mps %>%
  mutate(
    ul_z = theta_mcs + 1.96*se_mcs,
    ll_z = theta_mcs - 1.96*se_mcs,
    lgl_valpop_in_int_z = (ll_z<=val_pop)&(val_pop<=ul_z),
    ul_asifc = theta_asifc + 1.96*se_asifc,
    ll_asifc = theta_asifc - 1.96*se_asifc,
    lgl_valpop_in_int_asifc = (ll_asifc<=val_pop)&(val_pop<=ul_asifc)
  )


sum(df_out_mps$lgl_valpop_in_int_z)/n_trial
sum(df_out_mps$lgl_valpop_in_int_asifc)/n_trial

df_out_mps %>%
  group_by(lgl_valpop_in_int_z) %>%
  mutate(length_int_z = ul_z-ll_z) %>%
  summarize(mean(length_int_z))


df_out_mps %>%
  group_by(lgl_valpop_in_int_asifc) %>%
  mutate(length_int_asifc = ul_asifc-ll_asifc) %>%
  summarize(mean(length_int_asifc))


# MRS ---------------------------------------------------------------------

n_trial = 1

df_cov_mrs = foreach(icount(n_trial),
                     .packages=c('survey'),
                     .combine=rbind) %dopar% {

                       list_samps = draw_samp_designs(data_pop=dat_pop,
                                                      size_ref_strata,
                                                      size_ref_cluster)


                       #                form_outcome = ~enroll
                       #                form_poststrat = ~sch.wide

                       des_targ = list_samps$des_strat
                       des_2be_cal = list_samps$des_clust

                       list_formula_margins = list(form_poststrat)
                       list_targ_tot_marg = lapply(list_formula_margins,
                                                   FUN=targ_tot_from_des_marg,
                                                   design_target=des_targ)
                       est_out_mps = sim_m_rakes(m_target_sim=5,
                                                 design_2be_strat=des_2be_cal,
                                                 form_outcome=form_outcome,
                                                 list_targ_tot_marg=list_targ_tot_marg,
                                                 list_formula_margins=list_formula_margins)

                       cat_1_mps = lapply(est_out_mps,function(x){x[1,]})
                       out_pool_cat_1_mps = do.call(rbind,cat_1_mps)

                       # as if c
                       list_pop_marg_asifc = lapply(list_targ_tot_marg,FUN=function(x)subset(x,select=-c(SE)))

                       des_rake2_asifc = survey::rake(design = des_2be_cal,
                                                      control = list(maxit = 100),
                                                      sample.margins = list_formula_margins,
                                                      population.margins = list_pop_marg_asifc
                       )

                       est_asifc = data.frame(svytotal(x=form_outcome,
                                                       design=des_rake2_asifc,
                                                       na.rm=TRUE))
                       names(est_asifc) = c('theta_asifc','se_asifc')


                       est_mps = combine_est(out_pool_cat_1_mps)[,c('theta_mcs','se_root_T')]
                       names(est_mps) = c('theta_mcs','se_mcs')
                       out_ests = data.frame(est_mps,est_asifc)
                     }


df_out_mrs = df_cov_mrs %>%
  mutate(
    ul_z = theta_mcs + 1.96*se_mcs,
    ll_z = theta_mcs - 1.96*se_mcs,
    lgl_valpop_in_int_z = (ll_z<=val_pop)&(val_pop<=ul_z),
    ul_asifc = theta_asifc + 1.96*se_asifc,
    ll_asifc = theta_asifc - 1.96*se_asifc,
    lgl_valpop_in_int_asifc = (ll_asifc<=val_pop)&(val_pop<=ul_asifc)
  )


sum(df_out_mrs$lgl_valpop_in_int_z)/n_trial
sum(df_out_mrs$lgl_valpop_in_int_asifc)/n_trial

df_out_mrs %>%
  group_by(lgl_valpop_in_int_z) %>%
  mutate(length_int_z = ul_z-ll_z) %>%
  summarize(mean(length_int_z))


df_out_mrs %>%
  group_by(lgl_valpop_in_int_asifc) %>%
  mutate(length_int_asifc = ul_asifc-ll_asifc) %>%
  summarize(mean(length_int_asifc))
