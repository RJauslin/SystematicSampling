#' Distance matrix
#'
#' Return the distance matrix.
#'
#' @param X Coordinates data.frame
#'
#' @return distance matrix
#' @export
#' @examples
#' N <- 36
#' X <- as.matrix(expand.grid(1:sqrt(N),1:sqrt(N)))
#' distMat(X,tore = TRUE,toreBound = 6)
#'
distMat <- function(X,tore = FALSE,toreBound = 0){
  N <- nrow(X)
  d <- array(rep(0,N*N),c(N,N))
  if(tore != 0){
    for(i in 1:N){
      d[i,] <- distUnitk(as.matrix(X),k =i,tore = TRUE, toreBound = toreBound)
    }
  }else{
    for(i in 1:N){
      d[i,] <- distUnitk(as.matrix(X),k =i,tore = FALSE, toreBound = 0)
    }
  }

  return(d)
}
