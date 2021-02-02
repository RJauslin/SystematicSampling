#' Find the strata that have the smallest distance at the end
#'
#' @param X sptaial coordinates
#' @param pik_tmp temporary pik (unit k has a cutted pik)
#' @param tb
#' @param ref
#' @param bound
#' @param tore
#' @param shift
#' @param toreBound
#'
#' @return
#' @export
#'
#' @examples
#'
#' ############################# 1D
#'
#' rm(list = ls())
#' set.seed(1)
#' eps <- 1e-13
#' N <- 10
#' n <- 3
#' X <- as.matrix(seq(1,N,1))
#' tb = 0.001
#' pik <- rep(0.3,N)
#' ref <- 6
#' pik[ref] <- runif(1,min = eps,max = pik[ref] - eps)
#' tore = TRUE
#' toreBound = 10
#' l_Strata <- allStrata(X,pik,ref,tb)
#' u_Strata <- unionStrata(l_Strata,ref)
#' s <- goodStrata(u_Strata)
#' plot(s,X)
#'
#' ################################## 2D
#'
#' rm(list = ls())
#' set.seed(1)
#' eps <- 1e-13
#' N <- 36
#' n <- 8
#' pik <- rep(n/N,N)
#'
#'
#' tb <- runif(2)/100
#' X <- as.matrix(cbind(runif(N),runif(N)))
#' tore = FALSE
#' toreBound = FALSE
#' ref = 1
#' pik[ref] <- runif(1,min = eps,max = pik[ref] - eps)
#'
#' l_Strata <- allStrata(X,pik,ref,tb)
#' u_Strata <- unionStrata(l_Strata)
#' s <- goodStrata(u_Strata)
#' plot(s,X)
goodStrata <- function(u_Strata){

  eps <- 1e-13
  n <- length(u_Strata)
  dat <- data.frame(modif = rep(0,n),
                     pik_modif = rep(0,n),
                     d_ref_modif = rep(0,n),
                      ref = rep(0,n))
  for(i in 1:length(u_Strata)){
    dat[i,1] <- u_Strata[[i]]$modif
    dat[i,2] <- u_Strata[[i]]$pik_modif
    dat[i,3] <- u_Strata[[i]]$d_ref_modif
    dat[i,4] <- u_Strata[[i]]$ref
  }


  # find closest starta
  out <- which(min(dat$d_ref_modif) == dat$d_ref_modif)
  if(length(unique(dat$ref[out])) > 1){
    out <- out[which.min(dat$pik_modif[out])]
  }else{
    out <- out[1]
  }


  # out <- which.min(dat$d_ref_modif)

  # find farthest starta
  # out <- which.max(dat$d_ref_modif)


  #if the same unit is cutted differently we choose the one that the smallest pik_modif
  out_tmp <- which(dat$modif == dat$modif[out])
  if(length(out_tmp) > eps & length(out) > 1 ){
    out <- out_tmp[which.min(dat[out_tmp,]$pik_modif)]
  }


  return(u_Strata[[out]])
}
