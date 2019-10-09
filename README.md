MultCal2Sim
===========

The goal of MultCal2Sim is to perform calibration with the pre-computed estimates. MultCal2Sim will calibrate initial estimates from the first data source onto reference control-totals from the second data source. There are two choices of computation tools available, post-stratification and raking....

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

### Step 0: Computing the estimate totals

The Data Source 1 is `apistrat`, which is a sample (pre) stratified by type of school (Elementary/Middle/High School), and the Data Source 2 is `apiclus1`, which is a cluster sample of school districts.Our goal is to estimate

`est_tot_from_des_joint(form_additive,design_refer)`: estimate joint totals from a survey design project `est_tot_from_des_1marg(form_1_term_only,design_refer)`: estimate one marginal totals from a survey design project

``` r

# stratified sample via ?apistrat
des_targ  = svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

# # one-stage cluster sample via ?apiclus1
des_2be_cal = svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)

# helper function to compute estimated totals within joint strata
library(MultCal2Sim)
df_targ_tot_joint = est_tot_from_des_joint(form_additive=form_poststrat,
                                            design_refer=des_targ)
```

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

### Step 1: Simulating draws of the estimated strata control totals from the referenced data source

m=1,2,3,...,M draws are simulated for each strata s=1,2,3,...,S in the first step.It will return one simulation of either joint totals or marginal totals in a data frame.

The `sim_tot_from_est()` function will simulate totals from pre-existing estimates.

``` r
#simulate one draw
sim_tot_from_est(df_or_list_est_tot=df_targ_tot_joint,type_strata='joint',lgl_rej_neg_sim=TRUE)
#>   sch.wide both     Freq
#> 1       No   No 1341.197
#> 2      Yes   No 1046.802
#> 3       No  Yes    0.000
#> 4      Yes  Yes 3705.083

# simulate 10 draws for joint totals
list_sim_out_joint = lapply(1:10,FUN=sim_tot_from_est,df_or_list_est_tot=df_targ_tot_joint,type_strata='joint')

# 10 draws
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
#>   sch.wide both     Freq
#> 1       No   No 1019.631
#> 2      Yes   No 1052.204
#> 3       No  Yes    0.000
#> 4      Yes  Yes 4213.629
```

### Step 2: Calibrating the estimates from the first data source onto the simulated control totals

We will pick a computation tool post-stratification or raking to calibrate the initial estimate to each draw we get in the step 1. It will return to multiple intermediate estimates. Here we will use post-stratification as our tool.

The function `cal_2_sim()` will calibrate (poststratify or rake) an outcome onto simulted poststrata totals.

``` r

#using mcsp to calibrate the estimate to the first draw
cal_2_sim(des_2be_cal=des_2be_cal,
         df_or_list_sim=list_sim_out_joint[[1]],
         form_outcome=form_outcome,form_poststrat=form_poststrat,
         type_cal='mcsp')
#>          total       SE
#> enroll 3474302 295659.2

# repeat the task 10 times
list_cal_out_joint = lapply(list_sim_out_joint,FUN=cal_2_sim,
                      des_2be_cal=des_2be_cal,
                      form_outcome=form_outcome,
                      form_poststrat=form_poststrat,
                      type_cal='mcsp')

str(list_cal_out_joint)
#> List of 10
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3474302
#>   ..$ SE   : num 295659
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3562845
#>   ..$ SE   : num 319786
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3158581
#>   ..$ SE   : num 257218
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3449922
#>   ..$ SE   : num 293345
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3479203
#>   ..$ SE   : num 3e+05
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3348166
#>   ..$ SE   : num 306096
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3449026
#>   ..$ SE   : num 319050
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3552086
#>   ..$ SE   : num 315277
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3251262
#>   ..$ SE   : num 285263
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3459005
#>   ..$ SE   : num 282416
```

### Step 3: Computing the final point estimate and the standard error

The final point estimate is the average of intermediate calibrations, and the standard error will be returned as well.

The `combine_est` function will combine intermediate calibrated estimates

``` r
# combine estimates
combine_est(list_cal_out_joint)
#>   theta_mcs se_root_T        T_var           B       U_bar
#> 1   3418440  327211.8 107067541775 16612476184 88793817973
```

### Using Raking on marginal totals (MCSR)

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

#est_tot_from_des_1marg(form_1_term_only=~sch.wide + both,design_refer=des_targ)

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
#> 1       No  762.0467
#> 2      Yes 4961.8667
#> 
#> [[2]]
#>   both     Freq
#> 1   No 2300.159
#> 2  Yes 3907.497

# simulate 10 draws
list_sim_out_marg = lapply(1:10,FUN=sim_tot_from_est,df_or_list_est_tot=list_targ_tot_marg,type_strata='marginal')

# list of draws 10
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
#> 1       No 1031.919
#> 2      Yes 4998.147
#> 
#> [[2]]
#>   both     Freq
#> 1   No 2435.916
#> 2  Yes 3629.896

# use mcsr to compute intermediate estimates
list_cal_out_marg = lapply(list_sim_out_marg,FUN=cal_2_sim,
                            des_2be_cal=des_2be_cal,
                            form_outcome=form_outcome,
                            form_poststrat=list_form_marg,
                            type_cal='mcsr')

#step 3: combine the estimates and compute final uncertainty
combine_est(list_cal_out_marg)
#>   theta_mcs se_root_T        T_var          B       U_bar
#> 1   3468475  324098.4 105039751645 6559887727 97823875146
```

Overall MCS helper that wraps together steps 1-3
================================================

``` r

mcs(des_2be_cal=des_2be_cal,
    df_or_list_est_tot=list_targ_tot_marg,
    form_outcome=form_outcome,
    form_poststrat=list_form_marg,
    type_cal='mcsr',
    num_sim=50,
    lgl_rej_neg_sim=TRUE)
#>   theta_mcs se_root_T        T_var           B       U_bar
#> 1   3428760  345931.4 119668535554 25348400483 93813167061
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
