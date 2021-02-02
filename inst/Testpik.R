


rm(list = ls())
library(BalancedSampling)
library(WaveSampling)
library(sampling)
library(parallel)
library(MASS)
numCores <- detectCores()
numCores

cl <- makeCluster(detectCores())


# saveRDS(Xcoord,file = "C:/Users/jauslinr/switchdrive/SystematicSampling/SystematicSampling/inst/Xcoord.rds")
clusterEvalQ(cl,{
  library(BalancedSampling)
  library(sampling)
  devtools::load_all(".")
})




############# 1D
N <- 30
n <- 5
Xcoord <- as.matrix(seq(1,N,1))
pik <- inclusionprobabilities(runif(N),n = 5)
tb = 0.001
tore <- TRUE
toreBound <- N



############# 2D runif
# set.seed(1)
# N <- 50
# n <- 5
# Xcoord <- as.matrix(cbind(runif(N, min = 1,max = 10),runif(N,min = 1,max = 10)))
# # X <- Xcoord
# pik <- rep(n/N,N)
# tb <- rep(0,2)
# tore = TRUE
# toreBound = 10
# tb = runif(n = 2,min = -0.1,0.1)/100

# s <- systematic(Xcoord,pik,tb = tb,tore = tore,toreBound = toreBound,comment = TRUE)
# plot(Xcoord)
# points(Xcoord[s,],pch = 16)
#

############# 2D grid
# N <- 36
# n <- 6
# Xcoord <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
# pik <- rep(n/N,N)
# tore = TRUE
# toreBound = sqrt(N)

f1 <- function(n,Xcoord,pik,tb,tore,toreBound){
  return(systematic(Xcoord,pik = pik,tb = tb,tore = tore,toreBound = toreBound))
}

f21D <- function(n,pik){
  return(which(UPsystematic(pik)==1))
}

# f2 <- function(n,Xcoord,pik){
# return(lpm1(pik,Xcoord))
# }


SIM <- 1000
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
fun = f21D,pik = pik)

# l2 <- parLapply(cl = cl,
#                 X = 1:SIM,
#                 fun = f2,
#                 Xcoord = Xcoord,
#                 pik = pik)

p1 <- as.vector(table(do.call(c,l1))/SIM)
p2 <- as.vector(table(do.call(c,l2))/SIM)
p1
p2

sum(abs( (p1 - pik)/sqrt(pik*(1-pik)/SIM)) < 2)/N *100
sum(abs( (p2 - pik)/sqrt(pik*(1-pik)/SIM))< 2)/N *100
pik
stopCluster(cl)

library(ggplot2)
dat <- data.frame(x = Xcoord[,1],y = Xcoord[,2],size = p1,pik = pik)
ggplot()+
  geom_point(data = dat,aes(x = x,y = y,size = pik),pch = 1,alpha = 1,colour = "black")+
  geom_point(data = dat,aes(x = x,y = y,size = size,colour = size))+
  theme_bw()



