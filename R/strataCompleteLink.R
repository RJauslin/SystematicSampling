#' Title
#'
#' @param d
#' @param pik
#' @param k
#' @param cum
#'
#' @return
#' @export
#'
#' @examples
#'
#' rm(list = ls())
#' set.seed(6)
#' eps <- 1e-13
#' N <- 36
#' n <-  6
#' pik <- rep(n/N,N)
#' pik <- inclusionprobabilities(runif(N),n)
#'
#' tb <- runif(2,min = -0.001,max = 0.001)
#' tb
#' X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
#' tore = TRUE
#' toreBound = sqrt(N)
#' comment = TRUE
#' bound = 1
#'
#' ref <- 16
#' s <- ref
#' k = 9
#' pik_cutted <-  runif(1,min = eps,max = pik[s[1]] - eps)
#' pikstar <- pik
#' pikstar[s] <- pik_cutted
#' pikInit <- pik
#' pik <- pikstar
#'
#' strata <- strataCompleteLink(X,pik,ref,k,tb,pikInit)
#' strata <- strataCompleteLink(X,pik,ref,k,tb,pikInit)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
strataCompleteLink <- function(X,pik,ref,k,tb,pikInit,bound = 1.0,tore = FALSE,toreBound = -1){

  eps = 1e-13
  N <- nrow(X)
  #initializing centroid
  # centroid <- X[k,]
  d <- distRef(X,k,tb,tore = tore,toreBound = toreBound)
  d_ref <- distRef(X,ref,tb,tore = tore,toreBound = toreBound)


  # ties distance
  # r = rank(d,ties.method="min")
  # r_unique <- sort(unique(r))

  # intialistaion
  j <- 0
  cum = 0
  strata <- c()

  d_tmp <- d






  i_0 <- which(pik > eps)
  D_tmp <- matrix(rep(0,length(i_0)*length(i_0)),ncol = length(i_0),nrow = length(i_0))
  d_tmp <- d[i_0]
  iX <- c()

  # loop
  while(cum <= (bound - eps)){


    j = j + 1;
    # idx = which(r == r_unique[j])

    # idx <- which.min(d_tmp)
    idx <- which_min(d_tmp)
    if(length(idx) > 1){
      idx <- which( abs(D_tmp[1,]- min(d_tmp)) < 1e-7)
    }

    # print(pik[i_0[idx]])
    if(pik[i_0[idx]] > eps){
      iX <- c(iX,idx)
      strata <- c(strata,i_0[idx])
      cum = cum + sum(pik[i_0[idx]])
      # print(X[i_0,])
      D_tmp[j,] <-  distRef(as.matrix(X[i_0,]),ref = iX[length(iX)],tb = tb,tore = tore,toreBound = toreBound)
      # for(jjj in 1:length(strata)){
        # D_tmp[jjj,] <- distRef(X,ref = strata[jjj],tb = tb,tore = tore,toreBound = toreBound)
      # }
      d_tmp <- apply(D_tmp,MARGIN = 2,FUN = max)
      d_tmp[iX] <- 1e9

    }
    print(strata)


    }
  #
  # plot(X)
  # points(X[strata,],pch = 16)
  # print(cum)


  # if ref is inside strata
  if(any(ref == strata)){
    modif <- strata[which.max(d_ref[strata])]

    # calculate w
    w <- rep(0,N)
    w[strata] <- pik[strata]
    highbound <- sum(pik[strata])
    lowbound <- sum(w[-modif])

    w[modif] = pik[modif]/(highbound-lowbound)*(bound - lowbound)



    # in 1D possibly the modif unit has not enough to cut so weed to decreasingly remove
    if(any(w < -eps)){
      while(any(w < -eps)){
        strata <- strata[-which(strata == modif)]
        modif <- strata[which.max(d_ref[strata])]
        # calculate w
        w <- rep(0,N)
        w[strata] <- pik[strata]
        highbound <- sum(pik[strata])
        lowbound <- sum(w[-modif])

        w[modif] = pik[modif]/(highbound-lowbound)*(bound - lowbound)
        # print(w)
      }
    }

    if(!any(strata == k)){
      # "1D equal --- Strata does not contain k anymore at the end"
      return(NULL)
    }


    # strata without modif
    strata_wo_modif <- strata
    strata_wo_modif <- strata_wo_modif[-which(strata_wo_modif == modif)]
    strata_wo_modif <- strata_wo_modif[-which(strata_wo_modif == ref)]



    #direction of the strata ONLY in 1D for now...........
    strata <- orderRef(strata,d_ref[strata],ref)
    if(abs(diff(strata)[1]) > (bound + eps)){
      if(diff(strata)[1] > eps){
        direction <- FALSE
      }else{
        direction <- TRUE
      }
    }else if(diff(strata)[1] > eps){
      direction <- TRUE
    }else if(diff(strata)[1] < eps){
      direction <- FALSE
    }




    out <- list(w = w,
                pik = pikInit,
                cum = cum,
                strata = strata,
                strata_wo_modif = strata_wo_modif,
                # centroid = centroid,
                ref = ref,
                k = k,
                d_ref_modif = d_ref[modif],
                # d_ref_modif = d[ref],
                pik_modif = pik[modif] - w[modif],
                modif = modif,
                direction = direction)
    class(out) <- c("strata")
  }else{
    out <- NULL
  }


  return(out)
}
