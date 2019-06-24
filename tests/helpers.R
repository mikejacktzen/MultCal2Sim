# refactor / incorporate to the main functions ----------------------------
# mult_sim() mult_cal() combine_est()
# from these low level helper functions

# a_comp_targ_from_design_helper.R ----------------------------------------



# helper function to output joint target counts from survey design object
# result is 'tidy' data frame, row is unique strata combo
# stopifnot(c('Freq','SE') %in% names(df_targ_tot_joint))

targ_tot_from_des_joint = function(formula_additive,design_target){


  lgl_num = any(sapply((design_target$variables)[,labels(terms(formula_additive))],is.numeric))
  lgl_int = any(sapply((design_target$variables)[,labels(terms(formula_additive))],is.integer))

  if((lgl_num==TRUE)||(lgl_int==TRUE)){
    stop('please supply target variables as character or factor')
  }

  form_poststrat = formula_additive
  string_interact_poststrat = paste0("~interaction(",
                                     paste0(labels(terms(form_poststrat)),
                                            collapse=","),
                                     ")")
  # formula(string_interact_poststrat)

  # note: svytotal formula syntax for joint interaction
  # does require ~interaction(var1+var2)

  targ_tot_uncertain = as.data.frame(svytotal(formula(string_interact_poststrat),
                                              design=design_target))


  # 'tidy format with last column Freq'

  # use design_target to form xtab 'structure'
  # as skeleton for value place holder

  tidy_df_temp = data.frame(xtabs(form_poststrat,
                                  data=design_target$variables))


  # replace irrelevant Freq values of 'tidy_df_temp$Freq'
  # with relevant values from 'targ_tot_uncertain$total'
  tidy_df_temp$Freq = targ_tot_uncertain$total
  tidy_df_temp$SE = targ_tot_uncertain$SE

  df_targ_tot_joint = tidy_df_temp

  return(df_targ_tot_joint)
}

# targ_tot_from_des(~age_grp_gen,design_target=des_brfss)
# targ_tot_from_des(~age_grp_gen+w1race,design_target=des_brfss)
# targ_tot_from_des(~age_grp_gen+w1race+sex,design_target=des_brfss)
# des_brfss$variables %>% names()


# targ_tot_from_des(~age_grp_gen,design_target=des_brfss)
# targ_tot_from_des(~age_grp_gen+w1race,design_target=des_brfss)
# targ_tot_from_des(~age_grp_gen+w1race+sex,design_target=des_brfss)
# des_brfss$variables %>% names()



# function to simulate one set of margin(s)
# totals computed from 'design_target'
# result in list of margins
# stopifnot(c('Freq','SE') %in% names(df_targ_tot_joint))

targ_tot_from_des_marg = function(form_1_term_only,design_target){

  # lgl_chr = is.character((design_target$variables)[,labels(terms(form_1_term_only))])
  # lgl_factor = is.factor((design_target$variables)[,labels(terms(form_1_term_only))])
  #
  # if((lgl_chr==FALSE)&&(lgl_factor==FALSE)){
  #   stop('please supply target variables as character or factor')
  # }

  if(length(labels(terms(form_1_term_only))) > 1){
    stop("For marginals, do NOT specify more than 1 term in the formula. Please `?lapply` this function over the argument `list(~margin1,~margin2,...)`")
  }

  lgl_num = any(sapply((design_target$variables)[,labels(terms(form_1_term_only))],is.numeric))
  lgl_int = any(sapply((design_target$variables)[,labels(terms(form_1_term_only))],is.integer))

  if((lgl_num==TRUE)||(lgl_int==TRUE)){
    stop('please supply target variables as character or factor')
  }


  # ?svytable for convenient tidy structure
  # but no se
  df_svy_tab_struct = as.data.frame(svytable(form_1_term_only,
                                             design=design_target
  ))

  # redundant but cheap computation

  # ?svytotal to get se and attach to above tidy structure
  df_svy_tot_se = data.frame(svytotal(x=form_1_term_only,
                                      # x=~age_grp_gen,
                                      design=design_target,
                                      # design=des_brfss,
                                      na.rm=TRUE))

  df_svy_tab_struct$SE = df_svy_tot_se$SE
  df_svy_tab_struct

}

# # design_target=des_brfss
# lapply(list(~age_grp_gen),
#        FUN=targ_tot_from_des_marg,
#        design_target=des_brfss) %>% str()
#
# lapply(list(~age_grp_gen,~w1race),
#        FUN=targ_tot_from_des_marg,
#        design_target=des_brfss) %>% str()
#
# lapply(list(~age_grp_gen,~w1race,~sex),
#        FUN=targ_tot_from_des_marg,
#        design_target=des_brfss) %>% str()



# b_0_combine_est.R -------------------------------------------------------

# combine multiple calibrations  ---------------------------------------------

combine_est  = function(df_mult_est_and_se){

  M = nrow(df_mult_est_and_se)

  theta_mcs = sum(df_mult_est_and_se$total)/M

  U_hat <- df_mult_est_and_se$SE^2
  U_bar <- sum(U_hat)/M

  B_i <- (df_mult_est_and_se$total-theta_mcs)^2
  B <- sum(B_i)/(M-1)

  T_var <- U_bar + ((1+(1/M))*B)
  se_root_T = sqrt(T_var)


  # if using t dist reference
  # degf_tdist = (M-1)*((1+(U_bar/((1+1/M)*B)))^2)
  # gamma = (1+(1/M))*B/T_var
  # degf_comp = df_mult_est_and_se$degf_comp[1]
  # degf_obs = ((degf_comp+1)/(degf_comp+3))*(degf_comp)*(1-gamma)
  # degf_adj = 1/((1/degf_tdist) + (1/degf_obs))

  out_mcs = data.frame(theta_mcs=theta_mcs,
                       se_root_T=se_root_T,
                       T_var=T_var,
                       B=B,
                       U_bar=U_bar
                       # degf_tdist=degf_tdist,
                       # degf_adj=degf_adj
  )
  return(out_mcs)
}


# b_1_b_func_mps_mod.R ----------------------------------------------------

sim_m_poststrats = function(m_target_sim,
                            design_2be_strat,
                            lgl_rej_neg_sim=TRUE,
                            df_targ_tot_joint,
                            form_poststrat,
                            form_outcome
                            ){


  stopifnot(c('Freq','SE') %in% names(df_targ_tot_joint))

  est_out = vector(mode="list",length=m_target_sim)
  for(i in 1:m_target_sim){

    message(paste0('iter ',i))

    # df_targ_tot_joint
    vec_tot_est_mean = df_targ_tot_joint$Freq
    vec_tot_est_se = df_targ_tot_joint$SE

    vec_pop_sim = rnorm(n=length(vec_tot_est_mean),
                        mean=vec_tot_est_mean,
                        sd=vec_tot_est_se)

    # rej_neg_sim=FALSE
    if(lgl_rej_neg_sim==TRUE){
      # truncate to non neg

      # vec_pop_sim = c(-2,130377.3,-10)
      rej_neg_draw = function(vec_pop_sim){
        vec_pop_sim_resamp = vec_pop_sim

        ind_neg_samp = which(vec_pop_sim<0)

        for(j in ind_neg_samp){
          # i=1
          resamp = vec_pop_sim[j]
          while(resamp<0){
            resamp = rnorm(n=length(vec_tot_est_mean[j]),
                           mean=vec_tot_est_mean[j],
                           sd=vec_tot_est_se[j])
          }
          vec_pop_sim_resamp[j] = resamp
        }
        return(vec_pop_sim_resamp)
      }
      vec_pop_sim = rej_neg_draw(vec_pop_sim)
    }


    # make a copy targ est for temp sim
    df_tidy_sim = base::subset(df_targ_tot_joint, select=-c(SE))
    df_tidy_sim$Freq = vec_pop_sim

    # df1 = design_2be_strat$variables %>% xtabs(formula=form_poststrat) %>% as.data.frame()
    # left_join(df1,df_tidy_sim,by=c("age_grp_gen", "w1race"))
    # age group 3 race 2 in gen but not brfss

    # note: postStratify formula syntax for joint interaction
    # does NOT require ~interaction(var1+var2)
    # see ex last line of ?rake
    # uses 'additive' syntax eg ~var1+var2

    # poststrat ---------------------------------------------------------------

    des_strat2_uncertain <- survey::postStratify(design=design_2be_strat,
                                                 strata=form_poststrat,
                                                 partial=TRUE,
                                                 population=df_tidy_sim)
    # form_outcome = ~enroll
    # form_outcome=~yr.rnd
    est_temp = as.data.frame(svytotal(x=form_outcome,
                                      design=des_strat2_uncertain,
                                      na.rm=TRUE))

    # converting to dataframe sometimes auto looses SE colname
    # enforce second column name to be SE

    colnames(est_temp)[2] = "SE"

    degf_comp = degf(des_strat2_uncertain)

    est_temp$degf_comp = degf_comp


    # est_out[[i]] <- t(est_temp['total'])
    est_out[[i]] <- est_temp


    rm(est_temp)
    rm(vec_pop_sim)
    rm(df_tidy_sim)
    rm(des_strat2_uncertain)
  }
  return(est_out)
}


# b_2_b_func_mrs_mod.R ----------------------------------------------------

sim_m_rakes = function(m_target_sim,
                       design_2be_strat,
                       form_outcome,
                       lgl_rej_neg_sim=TRUE,
                       list_targ_tot_marg,
                       list_formula_margins
){

  est_out = vector(mode="list",length=m_target_sim)


  for(i in 1:m_target_sim){

    message(paste0('iter ',i))
    # can pre generate list of simulations
    # then lapply across simulations as arguments

    # var1

    # list_formula_margins=list(~age_grp_gen,~w1race)
    # form_1_term_only = list_formula_margins[[1]]
    # design_target = des_brfss

    # list_targ_tot_marg

    sim_marg_one = function(one_targ_tot,lgl_rej_neg_sim){

      stopifnot(c("Freq","SE") %in% names(one_targ_tot))

      # list_targ_tot_marg
      # one_targ_tot = list_targ_tot_marg[[1]]

      vec_tot_est_mean = one_targ_tot$Freq
      vec_tot_est_se = one_targ_tot$SE

      vec_pop_sim = rnorm(n=length(vec_tot_est_mean),
                          mean=vec_tot_est_mean,
                          sd=vec_tot_est_se)
      # rej_neg_sim=FALSE
      if(lgl_rej_neg_sim==TRUE){
        # truncate to non neg

        # vec_pop_sim = c(-2,130377.3,-10)
        rej_neg_draw = function(vec_pop_sim){
          vec_pop_sim_resamp = vec_pop_sim

          ind_neg_samp = which(vec_pop_sim<0)

          for(j in ind_neg_samp){
            # i=1
            resamp = vec_pop_sim[j]
            while(resamp<0){
              resamp = rnorm(n=length(vec_tot_est_mean[j]),
                             mean=vec_tot_est_mean[j],
                             sd=vec_tot_est_se[j])
            }
            vec_pop_sim_resamp[j] = resamp
          }
          return(vec_pop_sim_resamp)
        }
        vec_pop_sim = rej_neg_draw(vec_pop_sim)
      }

      df_pop_sim_var1 = base::subset(one_targ_tot,select=-c(SE))
      df_pop_sim_var1$Freq = vec_pop_sim
      return(df_pop_sim_var1)
    }

    list_pop_marg_sim = lapply(list_targ_tot_marg,
                               FUN=sim_marg_one,
                               lgl_rej_neg_sim=lgl_rej_neg_sim)

    # list_formula_margins = list(~age_grp_gen,~w1race)
    # apply for each margin var in list of formulas
    # list_pop_marg_sim = lapply(list_formula_margins,
    #                            FUN=sim_pop_from_samp_form,
    #                            design_target=design_target)


    # rake --------------------------------------------------------------------
    # list_samp_marg = list(~age_grp_gen,~w1race)
    # design_2be_strat = des_gen

    des_rake2_uncertain = survey::rake(design = design_2be_strat,
                                       control = list(maxit = 100),
                                       sample.margins = list_formula_margins,
                                       # population.margins = list(df_pop_sim_var1,
                                       #                           df_pop_sim_var2)
                                       population.margins = list_pop_marg_sim
    )

    est_temp = data.frame(svytotal(x=form_outcome,
                                   design=des_rake2_uncertain,
                                   na.rm=TRUE))

    colnames(est_temp)[2] = "SE"

    est_out[[i]] <- est_temp
    rm(est_temp)
    rm(list_pop_marg_sim)
    rm(des_rake2_uncertain)
  }

  return(est_out)
}


