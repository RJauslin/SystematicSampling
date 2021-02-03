#' Title
#'
#' @param SIM
#' @param Xcoord
#' @param pik
#' @param tb
#' @param tore
#' @param toreBound
#'
#' @return
#' @export
#'
#' @examples
#'
#' ############# 2D runif
#' rm(list = ls())
#' N <- 36
#' n <- 6
#' Xcoord <- as.matrix(cbind(runif(N, min = 1,max = 10),runif(N,min = 1,max = 10)))
#' pik <- rep(n/N,N)
#' tb <- rep(0,2)
#' tore = FALSE
#' toreBound = -1
#' SIM <- 200
#'
#' res <- simu(SIM,Xcoord,pik,tb,tore,toreBound)
#'
#' library(ggplot2)
#' dat <- data.frame(x = Xcoord[,1],y = Xcoord[,2],size = res$pik_systematic,pik = pik)
#'
#' ggplot()+
#'   geom_point(data = dat,aes(x = x,y = y,size = pik),pch = 1,alpha = 1,colour = "black")+
#'   geom_point(data = dat,aes(x = x,y = y,size = size,colour = size))+
#'   theme_bw()
#'
#'
#' ############# 2D grid
#' rm(list = ls())
#' N <- 36
#' n <- 6
#' Xcoord <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
#' pik <- rep(n/N,N)
#' tore = TRUE
#' toreBound = 6
#' tb <- runif(2)/100
#' SIM = 100
#' s <- systematic(Xcoord,pik,tb,tore = tore,toreBound = toreBound,comment = TRUE)
#' plot(Xcoord)
#' points(Xcoord[s,],pch = 16)
#'
#' res <- simu(SIM,Xcoord,pik,tb,tore,toreBound)
#'
#' library(ggplot2)
#' dat <- data.frame(x = Xcoord[,1],y = Xcoord[,2],size = res$pik_systematic,pik = pik)
#'
#' ggplot()+
#'   geom_point(data = dat,aes(x = x,y = y,size = pik),pch = 1,alpha = 1,colour = "black")+
#'   geom_point(data = dat,aes(x = x,y = y,size = size,colour = size))+
#'   theme_bw()
#'
#' ############# 1D
#' rm(list = ls())
#' N <- 30
#' n <- 5
#' Xcoord <- as.matrix(seq(1,N,1))
#' pik <- inclusionprobabilities(runif(N),n = 5)
#' tb = 0.001
#' tore <- TRUE
#' toreBound <- N
#' SIM = 1000
#' simu(SIM,Xcoord,pik,tb,tore,toreBound)
#'
simu <- function(SIM,
                 Xcoord,
                 pik,
                 tb,
                 tore,
                 toreBound){

  N <- nrow(Xcoord)
  n <- sum(pik)


  Wini <- try({
    W <- WaveSampling::wpik(Xcoord,pik,tore = tore,shift = FALSE,toreBound = toreBound)
    W <- W - diag(diag(as.matrix(W)))
    Winv <- wpikInv(Xcoord,pik,tore = tore,shift = FALSE,toreBound = toreBound)
  })
  if(class(Wini)== "try-error"){
    stopCluster(cl)
    stop("There is an error on generation of W.")
  }

  f1 <- function(n,Xcoord,pik,tb,tore,toreBound,W){
    s <- systematic(Xcoord,pik,tb,tore = tore,toreBound = toreBound,comment = FALSE)
    s_01 <- rep(0,nrow(Xcoord))
    s_01[s] <- 1
    v <- sb_vk(pik,Xcoord,s_01,tore = tore,toreBound = toreBound)
    sb <- round((1/length(s))*sum((v[which(v != 0)]-1)^2),5)


    return(list(measure = c(sb = sb,IB = IB(W,s_01), IBpikinv = IB(Winv,s_01)),
                s = s))
  }


  if(ncol(Xcoord) == 1){
    f2 <- function(n,Xcoord,pik,W){
      s_01 <- UPsystematic(pik)
      s <- which(s_01==1)
      v <- sb_vk(pik,Xcoord,s_01,tore = tore,toreBound = toreBound)
      sb <- round((1/length(s))*sum((v[which(v != 0)]-1)^2),5)
      return(list(measure = c(sb = sb,IB = IB(W,s_01),IBpikinv = IB(Winv,s_01)),
                  s = s))
     }
  }else{
    f2 <- function(n,Xcoord,pik,W){
      s <- lpm1(pik,Xcoord)
      s_01 <- rep(0,nrow(Xcoord))
      s_01[s] <- 1
      v <- sb_vk(pik,Xcoord,s_01,tore = tore,toreBound = toreBound)
      sb <-round((1/length(s))*sum((v[which(v != 0)]-1)^2),5)
      return(list(measure = c(sb = sb,IB = IB(W,s_01), IBpikinv = IB(Winv,s_01)),
                  s = s))
    }
  }



  numCores <- detectCores()
  numCores

  cl <- makeCluster(detectCores())

  clusterEvalQ(cl,{
    library(BalancedSampling)
    library(sampling)
    library(WaveSampling)
    devtools::load_all(".")
  })


  sim <- try({
    l1 <- parLapply(cl = cl,
                    X = 1:SIM,
                    fun = f1,
                    Xcoord = Xcoord,
                    pik = pik,
                    tb = tb,
                    tore = tore,
                    toreBound = toreBound,
                    W = W)
    l2 <- parLapply(cl = cl,
                    X = 1:SIM,
                    fun = f2,
                    Xcoord = Xcoord,
                    pik = pik,
                    W = W)
  })
  if(class(sim) == "try-error"){
    stopCluster(cl)
    stop("There is an error in simulations.")
  }



  m1 <- colMeans(do.call(rbind,lapply(l1,function(x){x$measure})))
  m2 <- colMeans(do.call(rbind,lapply(l2,function(x){x$measure})))

  p1 <- as.vector(table(do.call(c,lapply(l1,function(x){x$s})))/SIM)
  p2 <- as.vector(table(do.call(c,lapply(l2,function(x){x$s})))/SIM)


  prc1 <- sum(abs( (p1 - pik)/sqrt(pik*(1-pik)/SIM)) < 2)/N *100
  prc2 <- sum(abs( (p2 - pik)/sqrt(pik*(1-pik)/SIM)) < 2)/N *100



  stopCluster(cl)


  return(list(systematic = m1,m2 = m2,pik_systematic  =  p1,p2 = p2,prc1 = prc1,prc2 = prc2))
}
