library(dplyr)

# install.packages('sampling')
# install.packages('survey')

library(survey)
data(api,package = 'survey')
# ?api
# install.packages('mice')
# use a 1-imputed dataset as the population
library(mice)
# ?mice

data_pop = mice::complete(mice::mice(apipop,m=1))

# dat_pop = readRDS("rprojects/synth_2_uncert/scripts//dat_pop.rds")


# stype and dnum had no missing
size_ref_strata = xtabs(~stype,apistrat) %>% as.numeric()
size_ref_cluster = apiclus1$dnum %>% unique() %>% length()


# data_pop = dat_pop
# summary(data_pop)
library(sampling)

# draw stratified sample

# apistrat is a sample stratified by stype,
size_ref_strata = xtabs(~stype,apistrat) %>% as.numeric()

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
des_clust = survey::svydesign(id=~ID_unit,
                               weights=~wt,
                               data=samp_cluster)

# SRS WR

s = srswr(n=ceiling(nrow(data_pop)*0.15),N=nrow(data_pop))
samp_srswr = data_pop[s==1,]
dim(samp_srswr)

des_srswr = survey::svydesign(id=~0,
                               # weights=~wt,
                               data=samp_srswr)


# mcs daisy chain ---------------------------------------------------------

# mcs_21=mcs(ds2,ds1)
# mcs_321=mcs(ds3,ds2)

# install.packages('devtools')
# library(devtools)
# devtools::install_github("statsccpr/MultCal2Sim")
# library(MultCal2Sim)


ds1 = des_strat
ds2 = des_clust
ds3 = des_srswr


ds1$variables %>% head
ds1$variables$both

# estimated controls of ds1

# mcsp
form_poststrat_1 = ~both
form_outcome_2 = ~stype

# mcsr
form_poststrat_1 = list(~both)
list_targ_tot_marg_1 = lapply(form_poststrat_1,FUN=est_tot_from_des_joint,design_refer=ds1)


est_21 = mcs(des_2be_cal=ds2,
             form_outcome=form_outcome_2,
             # df_or_list_est_tot=df_targ_tot_joint_1,
             # type_cal='mcsp',
             form_poststrat=form_poststrat_1,
             df_or_list_est_tot=list_targ_tot_marg_1,
             type_cal='mcsr',
             # form_outcome=~enroll,
             num_sim=50,
             lgl_rej_neg_sim=TRUE)

est_21

# use mcs_21 categorical estimates as strata for another mcs

form_outcome_3 = ~enroll


# mcs_321(ds3,mcs_21)


# demo where est_21 is a single variable
# that is now used as estimated control for est_321

# user needs to manually subset and rename relevant output of mcs()
# to mimic use as 'df_or_list_est_tot'

# eg requires Freq and SE

df_targ_tot_joint_21 = est_21[,c("theta_mcs","se_root_T")]
colnames(df_targ_tot_joint_21) = c("Freq","SE")
est_21[,"outcome"]

# mcsp
df_targ_tot_joint_21 = data.frame(stype = c("E","H","M"),
                                  df_targ_tot_joint_21)

# mcsr
list_targ_tot_marg_21 = list(df_targ_tot_joint_21)

# NOTE:
# if the next est_321 = mcs(df_or_list_est_tot=,...) uses several estimated control variables
# in 'df_targ_tot_joint_21'
# user needs to manually get the several estimates of est_21
# and have the several variables reflected in form_outcome_2

est_321 = mcs(des_2be_cal=ds3,
              form_outcome=form_outcome_3,
              # type_cal='mcsp',
              # df_or_list_est_tot=df_targ_tot_joint_21,
              # form_poststrat=form_outcome_2,
              type_cal='mcsr',
              df_or_list_est_tot=list_targ_tot_marg_21,
              form_poststrat=list(form_outcome_2),
              # form_outcome=~enroll,
              # form_poststrat=~stype,
              num_sim=50,
              lgl_rej_neg_sim=TRUE)

est_321  # mcs(ds3,mcs(ds2,ds1))

svytotal(design=ds3,x=~enroll)  # design srswr
svytotal(design=ds2,x=~enroll)  # design cluster
svytotal(design=ds1,x=~enroll)  # design strata

sum(data_pop$enroll)  # true total

# but imagine enroll is not availble in neither ds1 nor ds2
# only available in ds3


