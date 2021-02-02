

rm(list = ls())
library(BalancedSampling)
library(WaveSampling)
library(parallel)
library(MASS)
library(sampling)
numCores <- detectCores()
numCores

cl <- makeCluster(detectCores())


# saveRDS(Xcoord,file = "C:/Users/jauslinr/switchdrive/SystematicSampling/SystematicSampling/inst/Xcoord.rds")
clusterEvalQ(cl,{
  library(BalancedSampling)
  library(WaveSampling)
  library(sampling)
  devtools::load_all(".")
})


N <- 36
n <- 6
Xcoord <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
pik <- rep(n/N,N)
pik <- inclusionprobabilities(runif(N),n)


tore = TRUE
toreBound = sqrt(N)
W <- wpik(Xcoord,pik,tore = TRUE,toreBound = 6)
W <- W - diag(diag(W))
tb = runif(n = 2,min = -0.1,0.1)/100

f1 <- function(n,Xcoord,pik,tb,tore,toreBound,W){
  s <- systematic(Xcoord,pik,tb,tore = tore,toreBound = toreBound,comment = FALSE)
  s_01 <- rep(0,nrow(Xcoord))
  s_01[s] <- 1
  return(list(measure = c(sb = sb(pik,Xcoord,s),IB = IB(W,s_01)),
              s = s))
}

f2 <- function(n,Xcoord,pik,W){
  s <- lpm1(pik,Xcoord)
  s_01 <- rep(0,nrow(Xcoord))
  s_01[s] <- 1
  return(list(measure = c(sb = sb(pik,Xcoord,s),IB = IB(W,s_01)),
              s = s))
}



SIM <- 200
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



colMeans(do.call(rbind,lapply(l1,function(x){x$measure})))
colMeans(do.call(rbind,lapply(l2,function(x){x$measure})))

p1 <- as.vector(table(do.call(c,lapply(l1,function(x){x$s})))/SIM)
p2 <- as.vector(table(do.call(c,lapply(l2,function(x){x$s})))/SIM)
pik

sum(abs( (p1 - pik)/sqrt(pik*(1-pik)/SIM)) < 2)/N *100
sum(abs( (p2 - pik)/sqrt(pik*(1-pik)/SIM)) < 2)/N *100



stopCluster(cl)

