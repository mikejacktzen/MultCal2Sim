

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
      cl <- makeCluster(getOption("cl.cores", num_core))
      # calibrate
      list_cal_out_joint = parLapply(cl = cl,
                                     X=list_sim_out_joint,
                                     fun=cal_2_sim,
                                     des_2be_cal=des_2be_cal,
                                     form_outcome=form_outcome,
                                     form_poststrat=form_poststrat,
                                     type_cal='mcsp')
      stopCluster(cl)

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
                               df_or_list_est_tot=list_targ_tot_marg,
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
      cl <- makeCluster(getOption("cl.cores", num_core))
      # calibrate
      list_cal_out_marg = parLapply(cl = cl,
                                    X=list_sim_out_marg,
                                    fun=cal_2_sim,
                                    des_2be_cal=des_2be_cal,
                                    form_outcome=form_outcome,
                                    form_poststrat=list_form_marg,
                                    type_cal='mcsr')
      stopCluster(cl)

    }
    # combine
    est_mcs = combine_est(list_cal_out_marg)
  }

  return(est_mcs)
}
