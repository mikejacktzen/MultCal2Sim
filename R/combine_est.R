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
  # list_cal = list_cal_out_joint
  M = length(list_cal)

  # check if outcome dimension is larger than 1 int/num or multiple levels of categorical
  lgl_gt_1_row = nrow(list_cal[[1]])>1

  # would be nice to just use array to handle both cases,
  # but need to differentiate use of colMeans() vs mean()
  # easier with ifelse

  if(lgl_gt_1_row==FALSE){

    df_mult_est_and_se = do.call(rbind,list_cal)

    # simple numeric 1 dim outcome
    est_outcome = df_mult_est_and_se$total
    se_est = df_mult_est_and_se$SE

    theta_mcs = mean(df_mult_est_and_se$total)
    B_i = (est_outcome-theta_mcs)^2
    B = sum(B_i)/(M-1)

    U_hat = se_est^2
    U_bar = mean(U_hat)

  }else{

    # lgl_gt_1_row==TRUE
    # categorical outcome with gt 1 level
    # work with array

    array_est = array(unlist(list_cal),dim = c(nrow(list_cal[[1]]),
                                               ncol(list_cal[[1]]),
                                               length(list_cal)))

    # each column is an estimate of a level of the outcome

    est_outcome = t(array_est[,1,])
    se_est = t(array_est[,2,])

    # theta_mcs = sum(df_mult_est_and_se$total)/M

    theta_mcs = colMeans(est_outcome)
    U_hat = se_est^2
    U_bar = colMeans(U_hat)


    # B_i = (est_outcome-theta_mcs)^2
    # est_outcome[,i] - theta_mcs[i]
    B_i = t(apply(est_outcome,1,'-',theta_mcs))^2

    # B = sum(B_i)/(M-1)
    B = colSums(B_i)/M-1

  }

  T_var = U_bar + ((1+(1/M))*B)
  se_root_T = sqrt(T_var)


  out_mcs = data.frame(name_outcome=rownames(list_cal[[1]]),
                       theta_mcs=theta_mcs,
                       se=se_root_T,
                       T_var=T_var,
                       B=B,
                       U_bar=U_bar)

  return(out_mcs)
}
