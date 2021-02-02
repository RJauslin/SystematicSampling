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
#' set.seed(1)
#' X <- as.matrix(cbind(runif(N, min = 1,max = 3),runif(N,min = 1,max = 3)))
#' ref <- which.min(X[,2])
#' d1 <- distRef(X,ref,tb = rep(0,2),tore = TRUE,toreBound = 3)
#' d2 <- distRef(X,ref,tb = rep(0,2),tore = FALSE,toreBound = 0)
#' plot(X)
#' points(X[which(abs(d1- d2) > 1e-7),],pch = 16)
#' points(X[which(d2 < 1),],pch = 16,col = "red")
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
