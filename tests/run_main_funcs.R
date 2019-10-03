# source("R/helpers.R")

source("R/est_tot_ref.R")
source("R/sim_tot_from_est.R")
source("R/cal_2_sim.R")
source("R/combine_est.R")

# install.packages('devtools')
# library(devtools)
# devtools::install_github('statsccpr/MultCal2Sim',force=TRUE)

?sim_tot_from_est
?cal_2_sim
?combine_est

# use ?api data example ------------------------------------------------------

library(survey)
data(api,package = 'survey')

form_outcome = ~enroll
form_poststrat = ~sch.wide+both

# stratified sample via ?apistrat
des_targ  = svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

# # one-stage cluster sample via ?apiclus1
des_2be_cal = svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)

# helper function to compute estimated totals within joint strata

est_tot_from_des_joint
est_tot_from_des_1marg

df_targ_tot_joint = est_tot_from_des_joint(form_additive=form_poststrat,
                                            design_refer=des_targ)



# test out the new main functions -----------------------------------------

# inputs
df_targ_tot_joint
form_poststrat
form_outcome
des_targ
des_2be_cal

# simulate
sim_tot_from_est(df_or_list_est_tot=df_targ_tot_joint,type_strata='joint',lgl_rej_neg_sim=TRUE)

list_sim_out_joint = lapply(1:10,FUN=sim_tot_from_est,df_or_list_est_tot=df_targ_tot_joint,type_strata='joint')
str(list_sim_out_joint,1)

cal_2_sim(des_2be_cal=des_2be_cal,
         df_or_list_sim=list_sim_out_joint[[1]],
         form_outcome=form_outcome,form_poststrat=form_poststrat,
         type_cal='mcsp')

# serial

list_cal_out_joint = lapply(list_sim_out_joint,FUN=cal_2_sim,
                      des_2be_cal=des_2be_cal,
                      form_outcome=form_outcome,
                      form_poststrat=form_poststrat,
                      type_cal='mcsp')
str(list_cal_out_joint)
# combine estimates
combine_est(list_cal_out_joint)

# system.time(list_cal_out <- lapply(list_sim_out,FUN=cal_2_sim,
#                       des_2be_cal=des_2be_cal,
#                       form_outcome=form_outcome,form_poststrat=form_poststrat))
#
## parallel
# cl <- makeCluster(getOption("cl.cores", 2))
# system.time(list_cal_out <- parLapply(cl = cl,
#                          X=list_sim_out,fun=cal_2_sim,
#                          des_2be_cal=des_2be_cal,
#                          # list_sim=list_sim_out,
#                          form_outcome=form_outcome,form_poststrat=form_poststrat))


# mcsr --------------------------------------------------------------------

# marginal targets

args(est_tot_from_des_1marg)
form_poststrat

# expect error if more than 1 RHS term in formula
form_poststrat
est_tot_from_des_1marg(form_1_term_only=~sch.wide + both,
                        design_refer=des_targ)

is.list(form_poststrat)

list_form_marg = list(~sch.wide,~both)
is.list(list_form_marg)

list_targ_tot_marg = lapply(list_form_marg,
                            FUN=est_tot_from_des_1marg,
                            design_refer=des_targ)



args(sim_tot_from_est)
sim_tot_from_est(df_or_list_est_tot=list_targ_tot_marg,type_strata='marginal',lgl_rej_neg_sim=TRUE)

# sim_tot_from_est(df_or_list_est_tot=df_targ_tot_joint,type_strata='joint',lgl_rej_neg_sim=TRUE)

list_sim_out_marg = lapply(1:10,FUN=sim_tot_from_est,df_or_list_est_tot=list_targ_tot_marg,type_strata='marginal')
str(list_sim_out_marg,2)

list_sim_out_marg = lapply(1:10,FUN=sim_tot_from_est,df_or_list_est_tot=list_targ_tot_marg,type_strata='marginal')

list_cal_out_marg = lapply(list_sim_out_marg,FUN=cal_2_sim,
                            des_2be_cal=des_2be_cal,
                            form_outcome=form_outcome,
                            form_poststrat=list_form_marg,
                            type_cal='mcsr')

combine_est(list_cal_out_marg)


# overall mcs helper ------------------------------------------------------

?mcs

mcs(des_2be_cal=des_2be_cal,
    df_or_list_est_tot=list_targ_tot_marg,
    form_outcome=form_outcome,
    form_poststrat=list_form_marg,
    type_cal='mcsr',
    num_sim=200,
    lgl_rej_neg_sim=TRUE)

# parallel
mcs(des_2be_cal=des_2be_cal,
    df_or_list_est_tot=list_targ_tot_marg,
    form_outcome=form_outcome,
    form_poststrat=list_form_marg,
    type_cal='mcsr',
    num_sim=200,
    parallel = TRUE,
    num_core = 4,
    lgl_rej_neg_sim=TRUE)



mcs(des_2be_cal=des_2be_cal,
    df_or_list_est_tot=df_targ_tot_joint,
    form_outcome=form_outcome,
    form_poststrat=form_poststrat,
    type_cal='mcsp',
    num_sim=200,
    parallel = TRUE,
    num_core = 4,
    lgl_rej_neg_sim=TRUE)

mcs(des_2be_cal=des_2be_cal,
    df_or_list_est_tot=df_targ_tot_joint,
    form_outcome=form_outcome,
    form_poststrat=form_poststrat,
    type_cal='mcsp',
    num_sim=200,
    lgl_rej_neg_sim=TRUE)
