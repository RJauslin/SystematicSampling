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
#' N <- nrow(X)
#' tb = runif(2)/100
#' #f <- function(k,Xcoord,tore,toreBound){distUnitk(as.matrix(Xcoord),k =k,tore, toreBound)}
#' #system.time(do.call(cbind,lapply(X = 1:N,FUN = f,Xcoord = X,tore = TRUE,toreBound = sqrt(N))))
#' system.time(D <- distMat(X,ref = 1,tb,tore = TRUE,toreBound = sqrt(N)))
#'
distMat <- function(X,ref,tb,tore = FALSE,toreBound = 0){
  N <- nrow(X)
  d <- array(rep(0,N*N),c(N,N))
  tmp = X[ref,]
  X[ref,] = X[ref,] + tb
  if(tore != 0){
    for(i in 1:N){
      d[i,] <- distUnitk(as.matrix(X),k =i,tore = TRUE, toreBound = toreBound)
    }
  }else{
    for(i in 1:N){
      d[i,] <- distUnitk(as.matrix(X),k =i,tore = FALSE, toreBound = 0)
    }
  }
  X[ref,] = tmp;
  return(d)
}
