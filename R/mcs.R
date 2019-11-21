
#' The 'mcs()' helper function combines the 3-steps: simulate, calibrate, combine
#'
#' @param des_2be_cal a survey design object that is to be calibrated see \code{\link[survey]{svydesign}}
#' @param df_or_list_est_tot a single data.frame of estimated joint totals or list of data.frames of estimated marginal totals (and respective standard errors) see \code{\link[MultCal2Sim]{sim_tot_from_est}}
#' @param form_outcome a formula of the outcome, whose total we want an improved MCS estimate of see \code{\link[MultCal2Sim]{cal_2_sim}}
#' @param form_poststrat single formula of joint poststrata or list of formulas for marginal poststrata see \code{\link[MultCal2Sim]{cal_2_sim}}
#' @param type_cal a character string for 'mcsp' or 'mcsr'
#' @param num_sim the number of simulations to calibrate onto
#' @param lgl_rej_neg_sim a logical whether to reject negative simulations
#' @param parallel a logical whether to run in parallel using \code{\link[parallel]{parLapply}}
#' @param num_core the number of cores for the parallel job
#'
#' @details
#'
#' @return a 1 row data.frame of the MCS estimate and its standard error
#' @export
#'
#' @examples
#'
mcs = function(des_2be_cal,
               df_or_list_est_tot,
               form_outcome,
               form_poststrat,
               type_cal,
               num_sim=1,
               parallel=FALSE,
               num_core=1,
               lgl_rej_neg_sim=TRUE){

  stopifnot(type_cal %in% c('mcsp','mcsr'))
  # assume precomputed reference totals supplied by 'df_or_list_est_tot'

  if(type_cal=='mcsp'){

    # simulate
    list_sim_out_joint = lapply(1:num_sim,FUN=sim_tot_from_est,
                                df_or_list_est_tot=df_or_list_est_tot,
                                type_strata='joint')

    if(parallel==FALSE){

      # calibrate
      list_cal_out_joint = lapply(list_sim_out_joint,
                                  FUN=cal_2_sim,
                                  des_2be_cal=des_2be_cal,
                                  form_outcome=form_outcome,
                                  form_poststrat=form_poststrat,
                                  type_cal='mcsp')
    }

    if(parallel==TRUE){
      cl <- parallel::makeCluster(getOption("cl.cores", num_core))
      # calibrate
      list_cal_out_joint = parallel::parLapply(cl = cl,
                                     X=list_sim_out_joint,
                                     fun=cal_2_sim,
                                     des_2be_cal=des_2be_cal,
                                     form_outcome=form_outcome,
                                     form_poststrat=form_poststrat,
                                     type_cal='mcsp')
      parallel::stopCluster(cl)

    }
    # combine estimates
    est_mcs = combine_est(list_cal_out_joint)
  }

  if(type_cal=='mcsr'){

    # list_form_marg = list(~sch.wide,~both)

    list_form_marg = form_poststrat
    stopifnot(is.list(list_form_marg))

    # simulate
    list_sim_out_marg = lapply(1:num_sim,
                               FUN=sim_tot_from_est,
                               df_or_list_est_tot=df_or_list_est_tot,
                               type_strata='marginal')

    if(parallel==FALSE){

      # calibrate
      list_cal_out_marg = lapply(list_sim_out_marg,
                                 FUN=cal_2_sim,
                                 des_2be_cal=des_2be_cal,
                                 form_outcome=form_outcome,
                                 form_poststrat=list_form_marg,
                                 type_cal='mcsr')
    }

    if(parallel==TRUE){
      cl <- parallel::makeCluster(getOption("cl.cores", num_core))
      # calibrate
      list_cal_out_marg = parallel::parLapply(cl = cl,
                                    X=list_sim_out_marg,
                                    fun=cal_2_sim,
                                    des_2be_cal=des_2be_cal,
                                    form_outcome=form_outcome,
                                    form_poststrat=list_form_marg,
                                    type_cal='mcsr')
      parallel::stopCluster(cl)

    }
    # combine
    est_mcs = combine_est(list_cal_out_marg)
  }

  return(est_mcs)
}
