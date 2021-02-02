#' Reorder the strata from ref perspective
#'
#' @param strata
#' @param d_ref
#'
#' @return
#' @export
#'
#' @examples
#' strata <- c(4, 5, 6)
#' d_ref <- c( 3.996001, 0.998001, 0.000000)
#' ref = 6
#' orderRef(strata,d_ref,ref)
#'
#' strata <- c(6, 7, 8, 1, 2)
#' d_ref <- c(0.000000,  0.998001,  3.996001,  8.994001, 15.992001)
#' ref = 6
#' orderRef(strata,d_ref,ref)
#'
orderRef <- function(strata,d_ref,ref){
  strata_tmp <- ref
  d_ref[which(strata == ref)] <- 1e9
  for(i in 2:length(strata)){
    k <- which.min(d_ref)
    strata_tmp <- c(strata_tmp,strata[k])
    d_ref[k] <- 1e9
  }
 return(strata_tmp)
}
