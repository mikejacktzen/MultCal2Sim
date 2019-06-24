#' The combine_est() function to combine intermediate calibrated estimates
#'
#' @param list_cal list containing intermediate estimates that have been calibrated by \code{\link[MultCal2Sim]{mult_cal}}
#'
#' @details
#'
#' @return a 1 row data.frame of the MCS estimate and its standard error
#' @export
#'
#' @examples
#'

combine_est = function(list_cal){

  df_mult_est_and_se = do.call(rbind,list_cal)

  M = nrow(df_mult_est_and_se)

  theta_mcs = sum(df_mult_est_and_se$total)/M

  U_hat <- df_mult_est_and_se$SE^2
  U_bar <- sum(U_hat)/M

  B_i <- (df_mult_est_and_se$total-theta_mcs)^2
  B <- sum(B_i)/(M-1)

  T_var <- U_bar + ((1+(1/M))*B)
  se_root_T = sqrt(T_var)


  out_mcs = data.frame(theta_mcs=theta_mcs,
                       se_root_T=se_root_T,
                       T_var=T_var,
                       B=B,
                       U_bar=U_bar
                       )

  return(out_mcs)

}
