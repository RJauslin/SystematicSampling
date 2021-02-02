#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' x <- c(1,2,3,4,6,1,3,5)
which_min <- function(x,eps = 1e-7){
  m1 <- min(x)
  return(which( abs(abs(x) - abs(m1)) < eps))
}

x <- c(1,2,3,4,6,1,3,5)
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' x <- c(1,2,3,4,6,1,3,5)
which_max <- function(x){
  m1 <- max(x)
  return(which(x == m1))
}
