
#' The cal_2_sim() function to calibrate (poststratify or rake) an outcome onto simulted poststrata totals
#'
#' @param des_2be_cal is a ?survey.design object created by ?survey::svydesign
#' @param df_or_list_sim 1 simulation of single data.frame (for joint) or single list of data.frames (for marginals)
#' @param form_outcome a formula of the outcome, whose total we want an improved MCS estimate of
#' @param form_poststrat single formula of joint poststrata or list of formulas for marginal poststrata
#' @param type_cal either mcsp or mcsr (MCS by poststratification or raking)
#'
#' @details
#'
#' @return a list of data.frames with c("total","SE") as columns
#' @export
#'
#' @examples
#'

cal_2_sim = function(des_2be_cal,
                    df_or_list_sim,
                    form_outcome,
                    form_poststrat,
                    # return_design=FALSE,  # eventual allow for regression estimates
                    type_cal
                    ){

  stopifnot(type_cal %in% c('mcsr','mcsp'))

  if(type_cal=='mcsp'){

    if(is.list(form_poststrat)==TRUE){
      stop(paste0('For mcsp, specify one formula with additive syntax `~pstrata1+pstrata2`. You supplied `form_poststrat=',
                  paste0("list(",paste0(as.character(form_poststrat),collapse=","),")")))
      }


    df_tidy_sim = df_or_list_sim

    # note: postStratify formula syntax for joint interaction
    # does NOT require ~interaction(var1+var2)
    # see ex last line of ?rake
    # uses 'additive' syntax eg ~var1+var2

    des_strat2_uncertain = survey::postStratify(design=des_2be_cal,
                                                strata=form_poststrat,
                                                partial=TRUE,
                                                population=df_tidy_sim)

    est_temp = as.data.frame(survey::svytotal(x=form_outcome,
                                              design=des_strat2_uncertain,
                                              na.rm=TRUE))

    # converting to dataframe sometimes auto looses SE colname
    # enforce second column name to be SE
    colnames(est_temp)[2] = "SE"

  }

  if(type_cal=='mcsr'){

    if(is.list(form_poststrat)==FALSE){
      stop(paste0('For mcsr, specify a list of 1-term formulas `list(~pstrat1,pstrat2)`. You supplied `form_poststrat=',
                  paste0(form_poststrat,collapse=""),"`"))
      }

    list_pop_marg_sim = df_or_list_sim

    des_rake2_uncertain = survey::rake(design = des_2be_cal,
                                       control = list(maxit = 100),
                                       sample.margins = form_poststrat,  # needs list format
                                       # population.margins = list(df_pop_sim_var1,
                                       #                           df_pop_sim_var2)
                                       population.margins = list_pop_marg_sim
    )

    est_temp = data.frame(survey::svytotal(x=form_outcome,
                                   design=des_rake2_uncertain,
                                   na.rm=TRUE))

    colnames(est_temp)[2] = "SE"

  }

  cal_out = est_temp
  return(cal_out)
}
