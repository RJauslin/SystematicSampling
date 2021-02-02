#' Distance from ref
#'
#' Return the refth row of the matrix
#'
#' @param X Coordinates data.frame
#'
#' @return distance matrix
#' @export
#' @examples
#' N <- 36
#' X <- as.matrix(expand.grid(1:sqrt(N),1:sqrt(N)))
#' distRef(X,ref = 4,tb = rep(0,2),tore = TRUE,toreBound = sqrt(N))
#'
distRef <- function(X,ref,tb,tore = FALSE,toreBound = 0){
  if(ncol(X) != length(tb)){
    stop("length of the shift is not equal to the number of column of the matrix coordinates.")
  }
  tmp = X[ref,]
  X[ref,] = X[ref,] + tb
  if(tore == TRUE){
    d_ref = distUnitk(X,ref,TRUE,toreBound);
  }else{
    d_ref = distUnitk(X,ref,FALSE,0.0);
  }
  X[ref,] = tmp;
  return(d_ref)
}
