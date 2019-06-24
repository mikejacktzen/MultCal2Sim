# ?sim_tot_from_est

#' The sim_tot_from_est() function to simulate totals from pre-existing estimates
#'
#' @param df_or_list_est_tot a single data.frame of estimated joint totals or list of data.frames of estimated marginal totals (and respective standard errors)
#' @param type_strata specify if joint (requires a dataframe) or marginal requires a list
#' @param lgl_rej_neg_sim logical to reject negative simulations of totals
#'
#' @return 1 simulation of either joint totals in a data.frame or marginal totals in a list of data.frames
#' @export
#'
#' @examples
#'

sim_tot_from_est = function(df_or_list_est_tot,
                    type_strata,
                    lgl_rej_neg_sim=TRUE){

  stopifnot(type_strata %in%c('joint','marginal'))

  # sim_targets() helper can be used for joint or lapply for marginals
  sim_targets = function(one_targ_tot,lgl_rej_neg_sim){

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

  if(type_strata=='joint'){
    if(is.data.frame(df_or_list_est_tot)==FALSE){stop("For `type_strata='joint'`, we need `df_or_list_est_tot` to be a single data.frame")}

    df_est_tot = df_or_list_est_tot
    df_tidy_sim = sim_targets(one_targ_tot=df_est_tot,lgl_rej_neg_sim=lgl_rej_neg_sim)
    return(df_tidy_sim)
  }

  if(type_strata=='marginal'){
    # hard to check if list of data.frames so ignore assertion checks

    # lapply() over list of marginal estimated totals
    list_pop_marg_sim = lapply(df_or_list_est_tot,
                               FUN=sim_targets,
                               lgl_rej_neg_sim=lgl_rej_neg_sim)

    return(list_pop_marg_sim)
  }
}
