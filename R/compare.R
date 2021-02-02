#' Chech if a vector is the same of another
#'
#' @param u
#' @param v
#' @param eps
#'
#' @return
#' @export
#'
#' @examples
#' compare(c(1,1),c(1,1))
#' compare(c(1,1),c(1,2))
compare <- function(u,v,eps = 1e-9){
  eps <- 1e-9
  out <- TRUE
  for(i in 1:length(u)){
    if(abs(u[i] -v[i]) > eps ){
      out <- FALSE
    }
  }
  return(out)
}
