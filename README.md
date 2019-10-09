MultCal2Sim
===========

The goal of MultCal2Sim is to perform calibration with the pre-computed estimates. MultCal2Sim will calibrate initial estimates from the first data source onto reference control-totals from the second data source. There are two choices of computation tools available, post-stratification and raking.

Installation
------------

You can install MultCal2Sim from github with:

``` r
# install.packages('devtools')
#library(devtools)
devtools::install_github("statsccpr/MultCal2Sim")
```

Example
-------

This is a basic example which shows you how to solve a common problem. We will use `survey::api`, a dataset providing infomation about student performance in California Schools.

``` r

library(survey)
#> Loading required package: grid
#> Loading required package: Matrix
#> Loading required package: survival
#> 
#> Attaching package: 'survey'
#> The following object is masked from 'package:graphics':
#> 
#>     dotchart
data(api,package = 'survey')


form_outcome = ~enroll
form_poststrat = ~sch.wide+both
```

We want an improved estimate of the total for the outcome `enroll` where `sch.wide` and `both` are strata variables.

The Data Source 1 is `apistrat`, which is a sample (pre) stratified by type of school (Elementary/Middle/High School), and the Data Source 2 is `apiclus1`, which is a cluster sample of school districts.

``` r

# stratified sample via ?apistrat
des_targ  = svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

# # one-stage cluster sample via ?apiclus1
des_2be_cal = svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
```

Step 0: Computing estimates for strata cell totals
--------------------------------------------------

If the user already has estimates for the strata cells, the user can skip to step 1. Otherwise, we have helper functions for the user to compute these strata cell estimates

`est_tot_from_des_joint(form_additive,design_refer)`: estimate joint totals from a survey design project `est_tot_from_des_1marg(form_1_term_only,design_refer)`: estimate marginal totals for a single variable from a survey design project

We'll first focus on the poststratification method, thus requiring joint control totals. Later we'll demonstrate how the marginal totals are used for the raking method.

``` r

# helper function to compute estimated totals within joint strata
library(MultCal2Sim)
df_targ_tot_joint = est_tot_from_des_joint(form_additive=form_poststrat,
                                            design_refer=des_targ)
```

We need the following objects

``` r
df_targ_tot_joint
#>   sch.wide both    Freq       SE
#> 1       No   No 1065.69 150.7916
#> 2      Yes   No 1170.74 183.4941
#> 3       No  Yes    0.00   0.0000
#> 4      Yes  Yes 3957.57 213.1103
form_poststrat
#> ~sch.wide + both
form_outcome
#> ~enroll
des_targ
#> Stratified Independent Sampling design
#> svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, 
#>     fpc = ~fpc)
des_2be_cal
#> 1 - level Cluster Sampling design
#> With (15) clusters.
#> svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
```

Step 1: Simulating draws using the estimated strata control totals from the reference data source
-------------------------------------------------------------------------------------------------

Simulate *m* = 1, 2, 3, ..., *M* draws for each strata cell *s* = 1, 2, 3, ..., *S* in the first step. The `sim_tot_from_est()` function will simulate totals from pre-existing estimates. It will return one simulation of either joint totals or marginal totals in a data frame.

``` r
#simulate one draw
sim_tot_from_est(df_or_list_est_tot=df_targ_tot_joint,type_strata='joint',lgl_rej_neg_sim=TRUE)
#>   sch.wide both      Freq
#> 1       No   No  935.6914
#> 2      Yes   No 1159.2189
#> 3       No  Yes    0.0000
#> 4      Yes  Yes 3815.4021

# simulate 10 draws for joint totals
list_sim_out_joint = lapply(1:10,FUN=sim_tot_from_est,df_or_list_est_tot=df_targ_tot_joint,type_strata='joint')

# list of 10 draws
str(list_sim_out_joint,1)
#> List of 10
#>  $ :'data.frame':    4 obs. of  3 variables:
#>  $ :'data.frame':    4 obs. of  3 variables:
#>  $ :'data.frame':    4 obs. of  3 variables:
#>  $ :'data.frame':    4 obs. of  3 variables:
#>  $ :'data.frame':    4 obs. of  3 variables:
#>  $ :'data.frame':    4 obs. of  3 variables:
#>  $ :'data.frame':    4 obs. of  3 variables:
#>  $ :'data.frame':    4 obs. of  3 variables:
#>  $ :'data.frame':    4 obs. of  3 variables:
#>  $ :'data.frame':    4 obs. of  3 variables:

# first draw
list_sim_out_joint[[1]]
#>   sch.wide both      Freq
#> 1       No   No 1145.3750
#> 2      Yes   No  975.3222
#> 3       No  Yes    0.0000
#> 4      Yes  Yes 3946.9113
```

Step 2: Calibrating the estimates from the first data source onto the simulated control totals
----------------------------------------------------------------------------------------------

The user can pick either post-stratification or raking to calibrate the initial estimate to each draw (simulated in the step 1). The function `cal_2_sim()` will calibrate (poststratify or rake) an outcome onto simulted poststrata totals. It will return a list of multiple intermediate estimates. Here we will use post-stratification as our tool.

``` r

#using mcsp to calibrate the estimate to the first draw
cal_2_sim(des_2be_cal=des_2be_cal,
         df_or_list_sim=list_sim_out_joint[[1]],
         form_outcome=form_outcome,form_poststrat=form_poststrat,
         type_cal='mcsp')
#>          total       SE
#> enroll 3359661 284404.1

# repeat the task 10 times via ?lapply()
list_cal_out_joint = lapply(list_sim_out_joint,FUN=cal_2_sim,
                      des_2be_cal=des_2be_cal,
                      form_outcome=form_outcome,
                      form_poststrat=form_poststrat,
                      type_cal='mcsp')

str(list_cal_out_joint)
#> List of 10
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3359661
#>   ..$ SE   : num 284404
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3356355
#>   ..$ SE   : num 296882
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3322311
#>   ..$ SE   : num 297511
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3209005
#>   ..$ SE   : num 275694
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3267514
#>   ..$ SE   : num 277714
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3301558
#>   ..$ SE   : num 293561
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3572887
#>   ..$ SE   : num 308643
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3357658
#>   ..$ SE   : num 296073
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3332468
#>   ..$ SE   : num 311331
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3568690
#>   ..$ SE   : num 330023
```

Step 3: Computing the final point estimate and the standard error
-----------------------------------------------------------------

The `combine_est` function will combine the list of intermediate estimates (from step 2). The final point estimate is the average of intermediate calibrations, and its standard error will be returned as well.

``` r
# combine estimates
combine_est(list_cal_out_joint)
#>   theta_mcs se_root_T       T_var           B      U_bar
#> 1   3364811  322355.4 1.03913e+11 13956354267 8.8561e+10
```

Using Raking on marginal totals (MCSR)
--------------------------------------

Alternative to poststratification onto joint totals, we demonstrate raking onto marginal totals. For raking, we require a list whose elements are one term formula (unlike poststratification requiring one formula of terms).

``` r

# marginal targets
args(est_tot_from_des_1marg)
#> function (form_1_term_only, design_refer) 
#> NULL
form_poststrat
#> ~sch.wide + both

# expect error if more than 1 RHS term in formula
form_poststrat
#> ~sch.wide + both

# est_tot_from_des_1marg(form_1_term_only=~sch.wide + both,design_refer=des_targ)

is.list(form_poststrat)
#> [1] FALSE

list_form_marg = list(~sch.wide,~both)
is.list(list_form_marg)
#> [1] TRUE

list_targ_tot_marg = lapply(list_form_marg,FUN=est_tot_from_des_1marg, design_refer=des_targ)

args(sim_tot_from_est)
#> function (df_or_list_est_tot, type_strata, lgl_rej_neg_sim = TRUE) 
#> NULL

# simulate one draw of margin(s)
sim_tot_from_est(df_or_list_est_tot=list_targ_tot_marg,type_strata='marginal',lgl_rej_neg_sim=TRUE)
#> [[1]]
#>   sch.wide      Freq
#> 1       No  936.4027
#> 2      Yes 5357.1989
#> 
#> [[2]]
#>   both     Freq
#> 1   No 2248.616
#> 2  Yes 4158.770

# simulate 10 draws
list_sim_out_marg = lapply(1:10,FUN=sim_tot_from_est,df_or_list_est_tot=list_targ_tot_marg,type_strata='marginal')

# list of draws
str(list_sim_out_marg,2)
#> List of 10
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>  $ :List of 2
#>   ..$ :'data.frame': 2 obs. of  2 variables:
#>   ..$ :'data.frame': 2 obs. of  2 variables:

# 5th draw
list_sim_out_marg[[5]]
#> [[1]]
#>   sch.wide     Freq
#> 1       No 1154.032
#> 2      Yes 4856.440
#> 
#> [[2]]
#>   both     Freq
#> 1   No 2583.685
#> 2  Yes 3720.849

# use mcsr to compute intermediate estimates
list_cal_out_marg = lapply(list_sim_out_marg,FUN=cal_2_sim,
                            des_2be_cal=des_2be_cal,
                            form_outcome=form_outcome,
                            form_poststrat=list_form_marg,
                            type_cal='mcsr')

#step 3: combine the estimates and compute final uncertainty
combine_est(list_cal_out_marg)
#>   theta_mcs se_root_T        T_var           B       U_bar
#> 1   3449268  367349.6 134945734481 34566738564 96922322060
```

Overall MCS helper that wraps together steps 1-3
------------------------------------------------

``` r

mcs(des_2be_cal=des_2be_cal,
    df_or_list_est_tot=list_targ_tot_marg,
    form_outcome=form_outcome,
    form_poststrat=list_form_marg,
    type_cal='mcsr',
    num_sim=50,
    lgl_rej_neg_sim=TRUE)
#>   theta_mcs se_root_T        T_var           B       U_bar
#> 1   3380969  346798.9 120269470311 30299342045 89364141425
```

`?mcs` has a parallel option

``` r

# parallel
mcs(des_2be_cal=des_2be_cal,
    df_or_list_est_tot=list_targ_tot_marg,
    form_outcome=form_outcome,
    form_poststrat=list_form_marg,
    type_cal='mcsr',
    num_sim=50,
    parallel = TRUE,
    num_core = 4,
    lgl_rej_neg_sim=TRUE)

# mcs(des_2be_cal=des_2be_cal,
#     df_or_list_est_tot=df_targ_tot_joint,
#     form_outcome=form_outcome,
#     form_poststrat=form_poststrat,
#     type_cal='mcsp',
#     num_sim=200,
#     parallel = TRUE,
#     num_core = 4,
#     lgl_rej_neg_sim=TRUE)
```
