MultCal2Sim
===========

The `MultCal2Sim` package for R has tools to perform calibration with pre-computed estimates. MultCal2Sim will calibrate initial estimates from the first data source onto estimated reference control-totals from the second data source. There are two choices of computation tools available, post-stratification and raking. For methodological details see \[@writeupmcs\].

Installation
------------

You can install MultCal2Sim from github with:

``` r
# install.packages('devtools')
# library(devtools)
devtools::install_github("statsccpr/MultCal2Sim")
```

Examples
========

We use the `survey::api` dataset that contains infomation about student performance in California Schools.

``` r
library(survey)
data(api,package = 'survey')


form_outcome = ~enroll
form_poststrat = ~sch.wide+both
```

We want an improved estimate of the total for the outcome `enroll` where `sch.wide` and `both` are post-strata variables.

The to-be-calibrated outcome is found in Data Source 1, `apiclus1`, which is a cluster sample of school districts. Acting as the target data source is Data Source 2, `apistrat`, which is a sample design (pre) stratified by type of school (Elementary/Middle/High School),

``` r
# stratified sample via ?apistrat
des_targ  = svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

# # one-stage cluster sample via ?apiclus1
des_2be_cal = svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
```

Poststratification
------------------

### Step 0: Computing estimates for strata cell totals

If the user already has estimates for the post-strata cells, the user can skip to step 1. Otherwise, we have helper functions for the user to compute the required post-strata cell estimates.

`est_tot_from_des_joint(form_additive,design_refer)`: estimate joint totals from a survey design object `est_tot_from_des_1marg(form_1_term_only,design_refer)`: estimate marginal totals for a single variable from a survey design object

We'll first focus on the MCS poststratification method, thus requiring joint control totals. Later we'll demonstrate how marginal totals are used for the MCS raking method.

``` r
# helper function to compute estimated totals within joint strata
library(MultCal2Sim)
df_targ_tot_joint = est_tot_from_des_joint(form_additive=form_poststrat,
                                            design_refer=des_targ)
```

Going forward, we need the following objects

``` r
df_targ_tot_joint  # data.frame with estimates of the post-strata cell totals
#>   sch.wide both    Freq       SE
#> 1       No   No 1065.69 150.7916
#> 2      Yes   No 1170.74 183.4941
#> 3       No  Yes    0.00   0.0000
#> 4      Yes  Yes 3957.57 213.1103
form_poststrat  # formula specifying poststrata
#> ~sch.wide + both
form_outcome   # formula specifying the outcome
#> ~enroll
des_2be_cal  # survey.design object containing the outcome to be calibrated
#> 1 - level Cluster Sampling design
#> With (15) clusters.
#> svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
```

### Step 1: Simulating draws using the estimated strata control totals from the target data source

Simulate *m* = 1, 2, 3, ..., *M* draws for each strata cell *s* = 1, 2, 3, ..., *S* in the first step. The `?sim_tot_from_est()` function will simulate totals from pre-existing estimates. It will return one simulation of either joint totals or marginal totals in a data frame.

``` r
# simulate one draw
sim_tot_from_est(df_or_list_est_tot=df_targ_tot_joint,type_strata='joint',lgl_rej_neg_sim=TRUE)
#>   sch.wide both     Freq
#> 1       No   No 1170.571
#> 2      Yes   No 1130.715
#> 3       No  Yes    0.000
#> 4      Yes  Yes 3921.192

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
#> 1       No   No  995.4881
#> 2      Yes   No 1205.8742
#> 3       No  Yes    0.0000
#> 4      Yes  Yes 4157.2966
```

### Step 2: Calibrating the estimates from the first data source onto the simulated control totals

The user can pick either post-stratification or raking to calibrate the initial estimate to each draw (simulated in step 1). The function `?cal_2_sim()` will calibrate (poststratify or rake) an outcome onto simulted poststrata totals. It will return a list of multiple intermediate estimates. Here we use post-stratification.

``` r
# using mcsp to calibrate the estimate to the first draw
cal_2_sim(des_2be_cal=des_2be_cal,
         df_or_list_sim=list_sim_out_joint[[1]],
         form_outcome=form_outcome,form_poststrat=form_poststrat,
         type_cal='mcsp')
#>          total     SE
#> enroll 3521337 310697

# repeat the task 10 times via ?lapply()
list_cal_out_joint = lapply(list_sim_out_joint,FUN=cal_2_sim,
                      des_2be_cal=des_2be_cal,
                      form_outcome=form_outcome,
                      form_poststrat=form_poststrat,
                      type_cal='mcsp')

str(list_cal_out_joint)
#> List of 10
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3521337
#>   ..$ SE   : num 310697
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3508682
#>   ..$ SE   : num 306333
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3308235
#>   ..$ SE   : num 267692
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3607524
#>   ..$ SE   : num 312647
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3382115
#>   ..$ SE   : num 296683
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3351404
#>   ..$ SE   : num 277309
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3329892
#>   ..$ SE   : num 278551
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3606328
#>   ..$ SE   : num 354130
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3533218
#>   ..$ SE   : num 309933
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3665366
#>   ..$ SE   : num 329621
```

### Step 3: Computing the final point estimate and the standard error

The `?combine_est()` function will combine the list of intermediate estimates (from step 2). The final point estimate is the average of intermediate calibrations, and its standard error will be returned as well.

``` r
# combine estimates
combine_est(list_cal_out_joint)
#>   theta_mcs se_root_T        T_var           B       U_bar
#> 1   3481410  334041.6 111583815649 16675465094 93240804045
```

Raking onto marginal totals (MCSR)
----------------------------------

Alternative to poststratification onto joint totals, we demonstrate raking onto marginal totals. For raking, the control variable specification requires a list where each element is a one term formula (unlike poststratification requiring one formula of terms).

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

# correct, list of one term formulae
list_form_marg = list(~sch.wide,~both)
is.list(list_form_marg)
#> [1] TRUE

list_targ_tot_marg = lapply(list_form_marg,FUN=est_tot_from_des_1marg, design_refer=des_targ)

args(sim_tot_from_est)
#> function (df_or_list_est_tot, type_strata, lgl_rej_neg_sim = TRUE) 
#> NULL

# step 1, simulate
# simulate one draw of margin(s)
sim_tot_from_est(df_or_list_est_tot=list_targ_tot_marg,type_strata='marginal',lgl_rej_neg_sim=TRUE)
#> [[1]]
#>   sch.wide      Freq
#> 1       No  959.3283
#> 2      Yes 5334.6884
#> 
#> [[2]]
#>   both     Freq
#> 1   No 2143.122
#> 2  Yes 4179.254

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
#>   sch.wide      Freq
#> 1       No  991.4722
#> 2      Yes 5167.2936
#> 
#> [[2]]
#>   both     Freq
#> 1   No 2052.733
#> 2  Yes 3720.079

# step 2, calibrate via mcsr to compute intermediate estimates
list_cal_out_marg = lapply(list_sim_out_marg,FUN=cal_2_sim,
                            des_2be_cal=des_2be_cal,
                            form_outcome=form_outcome,
                            form_poststrat=list_form_marg,
                            type_cal='mcsr')

# step 3: combine the estimates and compute final uncertainty
combine_est(list_cal_out_marg)
#>   theta_mcs se_root_T        T_var           B       U_bar
#> 1   3447281  344266.9 118519673026 20066237623 96446811641
```

Overall MCS helper that wraps together steps 1-3
------------------------------------------------

For the data arguments, the `?mcs()` wrapper needs a survey object with the outcome to be calibrated, the precomputed estimates of the control totals, a formula of the outcome, and a formula of the poststrata controls.

``` r
mcs(des_2be_cal=des_2be_cal,
    df_or_list_est_tot=list_targ_tot_marg,
    form_outcome=form_outcome,
    form_poststrat=list_form_marg,
    type_cal='mcsr',
    num_sim=50,
    lgl_rej_neg_sim=TRUE)
#>   theta_mcs se_root_T        T_var           B       U_bar
#> 1   3465124  345557.8 119410177285 23501860118 95438279965
```

The `?mcs()` wrapper has a parallel option

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

``` r
sessionInfo()
#> R version 3.3.3 (2017-03-06)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows Server 2012 R2 x64 (build 9600)
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.1252 
#> [2] LC_CTYPE=English_United States.1252   
#> [3] LC_MONETARY=English_United States.1252
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.1252    
#> 
#> attached base packages:
#> [1] grid      stats     graphics  grDevices utils     datasets  methods  
#> [8] base     
#> 
#> other attached packages:
#> [1] MultCal2Sim_0.1.0 survey_3.33-2     survival_2.41-3   Matrix_1.2-10    
#> 
#> loaded via a namespace (and not attached):
#>  [1] Rcpp_0.12.16    lattice_0.20-35 digest_0.6.14   rprojroot_1.3-2
#>  [5] backports_1.1.2 magrittr_1.5    evaluate_0.10.1 stringi_1.1.7  
#>  [9] rmarkdown_1.10  splines_3.3.3   tools_3.3.3     stringr_1.3.1  
#> [13] yaml_2.1.18     htmltools_0.3.6 knitr_1.20
```
