MultCal2Sim
===========

The goal of MultCal2Sim is to perform calibration with the pre-computed estimates. MultCal2Sim will calibrate initial estimates from one data source onto reference control-totals from a second data source. There are two choices of computation tools available, post-stratification and raking.

Installation
------------

You can install MultCal2Sim from github with:

``` r
# install.packages('devtools')
#library(devtools)
devtools::install_github("laurelhz/MultCal2Sim")
#?sim_tot_from_est
#?cal_2_sim
#?combine_est
```

Example
-------

This is a basic example which shows you how to solve a common problem. We'll use `survey::api`, a dataset providing infomation about student performance in California Schools. This dataset contains 6194 observations on 37 variables.

### Step 0: Compute the Estimate Totals

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

``` r
# stratified sample via ?apistrat
des_targ  = svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

# # one-stage cluster sample via ?apiclus1
des_2be_cal = svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)

# helper function to compute estimated totals within joint strata
library(MultCal2Sim)
?est_tot_from_des_joint
#> starting httpd help server ... done
?est_tot_from_des_1marg

df_targ_tot_joint = est_tot_from_des_joint(form_additive=form_poststrat,
                                            design_refer=des_targ)
```

### Step 1:Simulate draws of the strata control totals from the referenced data source

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

# simulate
sim_tot_from_est(df_or_list_est_tot=df_targ_tot_joint,type_strata='joint',lgl_rej_neg_sim=TRUE)
#>   sch.wide both     Freq
#> 1       No   No 1119.774
#> 2      Yes   No 1328.235
#> 3       No  Yes    0.000
#> 4      Yes  Yes 3939.573

list_sim_out_joint = lapply(1:10,FUN=sim_tot_from_est,df_or_list_est_tot=df_targ_tot_joint,type_strata='joint')
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
```

### Step 2: Calibrate the estimates from the first data source onto the simulated control totals

``` r
cal_2_sim(des_2be_cal=des_2be_cal,
         df_or_list_sim=list_sim_out_joint[[1]],
         form_outcome=form_outcome,form_poststrat=form_poststrat,
         type_cal='mcsp')
#>          total     SE
#> enroll 3188432 271754

# serial

list_cal_out_joint = lapply(list_sim_out_joint,FUN=cal_2_sim,
                      des_2be_cal=des_2be_cal,
                      form_outcome=form_outcome,
                      form_poststrat=form_poststrat,
                      type_cal='mcsp')
str(list_cal_out_joint)
#> List of 10
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3188432
#>   ..$ SE   : num 271754
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3302949
#>   ..$ SE   : num 270674
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3185587
#>   ..$ SE   : num 294107
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3262455
#>   ..$ SE   : num 297726
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3806969
#>   ..$ SE   : num 362092
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3596084
#>   ..$ SE   : num 316276
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3730425
#>   ..$ SE   : num 333521
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3314064
#>   ..$ SE   : num 290398
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3722966
#>   ..$ SE   : num 322673
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3308579
#>   ..$ SE   : num 256499
```

### Step 3: Compute the final point estimate and the standard error

``` r
# combine estimates
combine_est(list_cal_out_joint)
#>   theta_mcs se_root_T       T_var           B       U_bar
#> 1   3441851    396559 1.57259e+11 59428840182 91887285513


str(list_cal_out_joint)
#> List of 10
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3188432
#>   ..$ SE   : num 271754
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3302949
#>   ..$ SE   : num 270674
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3185587
#>   ..$ SE   : num 294107
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3262455
#>   ..$ SE   : num 297726
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3806969
#>   ..$ SE   : num 362092
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3596084
#>   ..$ SE   : num 316276
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3730425
#>   ..$ SE   : num 333521
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3314064
#>   ..$ SE   : num 290398
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3722966
#>   ..$ SE   : num 322673
#>  $ :'data.frame':    1 obs. of  2 variables:
#>   ..$ total: num 3308579
#>   ..$ SE   : num 256499
# combine estimates
combine_est(list_cal_out_joint)
#>   theta_mcs se_root_T       T_var           B       U_bar
#> 1   3441851    396559 1.57259e+11 59428840182 91887285513
```
