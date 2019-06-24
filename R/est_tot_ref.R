
#' helper function to estimate joint totals from survey design object
#'
#' @param form_additive a formula with only right hand side additive terms representing joint totals
#' @param design_refer a survey.design object that contains the survey data used to estimate the joint total(s) \code{\link[survey]{svydesign}}
#'
#' @details
#'
#' @return result is 'tidy' data frame, row is unique strata combo
#' @export
#'
#' @examples
#'
#' dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' est_tot_from_des_joint(~sch.wide+bothdesign_refer=dstrat)

est_tot_from_des_joint = function(form_additive,design_refer){


  lgl_num = any(sapply((design_refer$variables)[,labels(terms(form_additive))],is.numeric))
  lgl_int = any(sapply((design_refer$variables)[,labels(terms(form_additive))],is.integer))

  if((lgl_num==TRUE)||(lgl_int==TRUE)){
    stop('please supply target variables as character or factor')
  }

  form_poststrat = form_additive
  string_interact_poststrat = paste0("~interaction(",
                                     paste0(labels(terms(form_poststrat)),
                                            collapse=","),
                                     ")")

  # note: svytotal formula syntax for joint interaction
  # does require ~interaction(var1+var2)

  est_tot_uncertain = as.data.frame(svytotal(formula(string_interact_poststrat),
                                              design=design_refer))


  # 'tidy format with last column Freq'

  # use design_refer to form xtab 'structure'
  # as skeleton for value place holder

  tidy_df_temp = data.frame(xtabs(form_poststrat,
                                  data=design_refer$variables))


  # replace irrelevant Freq values of 'tidy_df_temp$Freq'
  # with relevant values from 'est_tot_uncertain$total'

  tidy_df_temp$Freq = est_tot_uncertain$total
  tidy_df_temp$SE = est_tot_uncertain$SE

  df_est_tot_joint = tidy_df_temp

  return(df_est_tot_joint)
}


#' helper function to estimate ONE marginal total from a survey design object
#'
#' @param form_1_term_only a formula with only 1 term on the right hand side representing a marginal total
#' @param design_refer a ?survey::design object that contains the survey data used to estimate the marginal total
#'
#' @return result is 'tidy' data frame
#' @export
#'
#' @examples
#' dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' est_tot_from_des_1marg(~sch.wide,design_refer=dstrat)
#' est_tot_from_des_1marg(~both,design_refer=dstrat)
#' lapply(list(~sch.wide,~both),est_tot_from_des_1marg,design_refer=dstrat)
#'
# expect error using more than 1 RHS term
# est_tot_from_des_1marg(~sch.wide+both,design_refer=dstrat)

est_tot_from_des_1marg = function(form_1_term_only,design_refer){


  if(length(labels(terms(form_1_term_only))) > 1){
    stop("For marginals, do NOT specify more than 1 term in the formula. Please `?lapply` this function over the argument `list(~margin1,~margin2,...)`")
  }

  lgl_num = any(sapply((design_refer$variables)[,labels(terms(form_1_term_only))],is.numeric))
  lgl_int = any(sapply((design_refer$variables)[,labels(terms(form_1_term_only))],is.integer))

  if((lgl_num==TRUE)||(lgl_int==TRUE)){
    stop('please supply target variables as character or factor')
  }


  # ?svytable for convenient tidy structure
  # but no se
  df_svy_tab_struct = as.data.frame(svytable(form_1_term_only,
                                             design=design_refer
  ))

  # ?svytotal to get se and attach to above tidy structure
  # redundant but cheap computation

  df_svy_tot_se = data.frame(svytotal(x=form_1_term_only,
                                      # x=~age_grp_gen,
                                      design=design_refer,
                                      # design=des_brfss,
                                      na.rm=TRUE))

  df_svy_tab_struct$SE = df_svy_tot_se$SE
  df_svy_tab_struct

}

