#' Create the union of the strata
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
#'
#' ############################# 1D
#'
#' rm(list = ls())
#' set.seed(1)
#' eps <- 1e-13
#' N <- 10
#' n <- 3
#' X <- as.matrix(seq(1,N,1))
#' tb = -0.001
#' pik <- rep(0.3,N)
#' pikInit <- pik
#' ref <- 6
#' pik[ref] <- runif(1,min = eps,max = pik[ref] - eps)
#' shift = TRUE
#' tore = TRUE
#' toreBound = 10
#' l_Strata <- allStrata(X,pik,ref,tb,pikInit)
#' l_Strata
#' u_Strata <- unionStrata(l_Strata,ref)
#' u_strata
#'
#' ################################# 2D
#' rm(list = ls())
#' set.seed(1)
#' N <- 36
#' n <- 8
#' pik <- rep(n/N,N)
#' pikInit <- pik
#'
#'
#' tb <- runif(2)/100
#' X <- as.matrix(cbind(runif(N),runif(N)))
#' tore = FALSE
#' toreBound = FALSE
#' ref = 1
#'
#' l_Strata <- allStrata(X,pik,ref,tb,pikInit)
#' plot(l_Strata,X)
#' u_Strata <- unionStrata(l_Strata)
#' plot(u_Strata,X)
#'
#' ################## GRID 2D
#'
#' rm(list = ls())
#' set.seed(1)
#' eps <- 1e-13
#' N <- 81
#' n <- 9
#' pik <- rep(n/N,N)
#'
#'
#' tb <- runif(2)/100
#'
#' X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
#' tore = TRUE
#' toreBound = sqrt(N)
#'
#' ref = 25
#'
#' l_Strata <- allStrata(X,pik,ref,tb,tore = tore,toreBound = toreBound,pikInit = pik)
#' plot(l_Strata,X)
#' u_Strata <- unionStrata(l_Strata)
#' plot(u_Strata,X)
unionStrata <- function(l_Strata,ref){

  eps <- 1e-13
  u_wo_modif <- c()
  u_modif <- c()
  u_strata <- c()
  for(i in 1:length(l_Strata)){
    u_strata <- c(u_strata, l_Strata[[i]]$strata)
    u_wo_modif  <- c(u_wo_modif, l_Strata[[i]]$strata_wo_modif)
    u_modif <- c(u_modif, l_Strata[[i]]$modif)

  }

  u_strata <- sort(unique(u_strata))
  u_wo_modif <- sort(unique(u_wo_modif))
  u_modif <- sort(unique(u_modif))


  # find the candidate strata
  candidate <- c()
  for(j in 1:length(u_modif)){
    if(!any(u_modif[j] == u_wo_modif)){
      candidate <- c(candidate,u_modif[j])
    }
  }

  #if candidate is null we are selecting the one that have the minimum distance ....
  # NOT SURE ABOUT THAT
  # normally we take min of max as here max does not exist we have to then take max
  #

  if(is.null(candidate)){

    d_dat <- do.call(rbind,lapply(l_Strata,FUN = function(x){return(x$d_ref_modif)}))
    m_d <- max(d_dat)
    smaller <- which(abs(m_d - d_dat) < 1e-10 )
    # print(smaller)
    # tmp2 <- 1e-9
    # for(i in 1:length(l_Strata)){
    #   if(tmp2 < l_Strata[[i]]$d_ref_modif){
    #     tmp2 <- l_Strata[[i]]$d_ref_modif
    #     smaller <- i
    #   }
    # }
    u_strata <- l_Strata[smaller]

  }else{

    u_strata <- list()
    for(k in 1:length(candidate)){
      u_strata_k <- list()
      for(l in 1:length(l_Strata)){
        if(l_Strata[[l]]$modif == candidate[k]){
          u_strata_k <- c(u_strata_k, l_Strata[l])
        }
      }
      # if we have two different value for modif then it means that the strata is in fact contain in two different strata
      # we hence keep the one that have the biggest w meaning that we cover the other parts.

      val_modif_k <- sapply(u_strata_k, function(x){as.numeric(x$w[x$modif])})
      if(length(unique(round(val_modif_k,10))) > 1){
        index <- which(max(val_modif_k) == val_modif_k)
        u_strata_k <- u_strata_k[index]
      }
      u_strata <- c(u_strata,u_strata_k)
    }

    # u_strata <- list()
    # tt <- 1
    # for(k in 1:length(candidate)){
    #   for(l in 1:length(l_Strata)){
    #     if(l_Strata[[l]]$modif == candidate[k]){
    #       u_strata[[tt]] <- l_Strata[[l]]
    #       tt <- tt + 1
    #     }
    #   }
    # }
  }



  class(u_strata) <- c("u_strata","l_strata")
  return(u_strata)
}
