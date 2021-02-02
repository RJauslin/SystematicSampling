

rm(list = ls())
library(BalancedSampling)
library(WaveSampling)
library(parallel)
library(MASS)
numCores <- detectCores()
numCores

cl <- makeCluster(detectCores())


# saveRDS(Xcoord,file = "C:/Users/jauslinr/switchdrive/SystematicSampling/SystematicSampling/inst/Xcoord.rds")
clusterEvalQ(cl,{
  library(BalancedSampling)
  devtools::load_all(".")
})


N <- 50
n <- 5
Xcoord <- as.matrix(cbind(runif(N, min = 1,max = 3),runif(N,min = 1,max = 3)))
pik <- rep(n/N,N)
tb <- rep(0,2)
tore = FALSE
toreBound = -1

f1 <- function(n,Xcoord,pik,tb,tore,toreBound){
  s <- systematic(Xcoord,pik,tb,tore = tore,toreBound = toreBound,comment = FALSE)
  return(sb(pik,Xcoord,s))
}

f2 <- function(n,Xcoord,pik,tb,tore,toreBound){
  s <- lpm1(pik,Xcoord)
  return(sb(pik,Xcoord,s))
}



SIM <- 100
l1 <- parLapply(cl = cl,
          X = 1:SIM,
          fun = f1,
          Xcoord = Xcoord,
          pik = pik,
          tb = tb,
          tore = tore,
          toreBound = toreBound)
l2 <- parLapply(cl = cl,
                X = 1:SIM,
                fun = f2,
                Xcoord = Xcoord,
                pik = pik,
                tb = tb,
                tore = tore,
                toreBound = toreBound)

mean(do.call(rbind,l1))
mean(do.call(rbind,l2))

stopCluster(cl)

















############################# 2D GRID



rm(list = ls())
eps <- 1e-13
N <- 144
n <-  24





pik <- rep(n/N,N)


tb <- runif(n = 2,min = -0.1,0.1)/100

X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
tore = TRUE
toreBound = sqrt(N)
comment = TRUE


W <- wpik(X,pik,tore = TRUE,shift = TRUE,toreBound = toreBound)
W <- W - diag(diag(W))
tb
s <- Systest5(X,pik,tb,tore = tore,toreBound = toreBound,comment = comment)
s_01 <- rep(0,N)
s_01[s] <- 1
sb(pik,X,s)
IB(W,s_01)

s_lpm1 <- lpm1(pik,X)
plot(X)
points(X[s,],pch = 16)
sb(pik,X,s_lpm1)

plot(X)
points(X[s_lpm1,],pch = 16)
