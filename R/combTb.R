#' Cives the all opposite side of a vector
#'
#' @param tb vector
#'
#' @return
#' @export
#'
#' @examples
#'
#' tb <- runif(2)/10
#' tbl <- combTb(tb)
#'
#' X <- matrix(c(1,1),nrow = 1)
#'
#'
#' plot(X)
#' for(i in 1:nrow(tbl)){
#' lines(X + tbl[i,],type = "p",col = i)
#' }
#'
combTb <- function(tb){

  # 1D opposite side
  if(length(tb)==1){
    out <- matrix(rep(tb,2),ncol = 1,byrow = T)
    out[2,] <- -1*out[1,]
  }

  # 2D four rotation of the vector
  if(length(tb) == 2){
    R <- matrix(c(0,-1,1,0),ncol = 2)
    out <- matrix(rep(tb,4),ncol = 2,byrow = T)
    out[2,] <- R%*%out[1,]
    out[3,] <- R%*%out[2,]
    out[4,] <- R%*%out[3,]
  }



  return(out)

}
