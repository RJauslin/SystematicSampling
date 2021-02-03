X <- as.matrix(cbind(runif(50),runif(50)))
pik <- rep(10/50,50)
pik <- inclusionprobabilities(runif(50),10)
s <- wave(X,pik)

plot(X)
points(X[s == 1,],pch = 16)

v <- sb_vk(pik,X,s)
v2 <- sb_vk3(pik,X,s)
v
v2
1/10*sum((v[which(v != 0)]-1)^2)
1/10*sum((v2[which(v2 != 0)]-1)^2)

grid <- cbind(x = seq(0,1,length.out = 100),y = rep(0,100))
grid <- rbind(grid,cbind(x = rep(1,100), y = seq(0,1,length.out = 100)))
grid <- rbind(grid,cbind(x =  seq(1,0,length.out = 100), y = rep(1,100)))
grid <- rbind(grid,cbind(x =  rep(0,100), y =seq(1,0,length.out = 100)))
grid <- cbind(grid , group = rep(1,400))
grid <- as.data.frame(grid)
dat <- data.frame(x = X[,1],y = X[,2],v = v,s = s)
ggplot()+
geom_voronoi(data = dat[dat$s ==1,],aes(x = x,y = y,fill = v),outline = grid ,size = 0.1,alpha = 1,colour ="black")+
 geom_point(data = dat,aes(x = x,y = y),shape = 1,size = 1,alpha = 0.5)+
 geom_point(data = dat[dat$s ==1,],aes(x = x,y = y),shape = 16,size = 1,alpha = 1)




rm(list = ls())
set.seed(1)
eps <- 1e-13
N <- 144
n <-  24
pik <- rep(n/N,N)
#pik <- sampling::inclusionprobabilities(runif(N),n)

tb <- runif(2)/100

X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
tore = TRUE
toreBound = sqrt(N)
comment = FALSE
s <- systematic(X,pik,tb,tore = tore,toreBound = toreBound,comment = TRUE)
s_01 <- rep(0,N)
s_01[s] <- 1
s1 <- s
v <- SystematicSampling::sb_vk(pik,X,s_01,tore = TRUE,toreBound = 12)
v3 <- sb_vk3(pik,X,s_01,tore = FALSE,toreBound = 1)
v2 <- sb_vk2(pik,X,s_01,tore = TRUE,toreBound = 12)


# v2 <- sb_vk3(pik,X,s_01,tore = TRUE,toreBound = toreBound)
as.vector(v)
as.vector(v2)
as.vector(v3)
1/6*sum((v[which(v != 0)]-1)^2)
1/6*sum((v2[which(v2 != 0)]-1)^2)
BalancedSampling::sb(pik,X,which(s_01 == 1))


grid <- cbind(x = seq(0,12,length.out = 100),y = rep(0,100))
grid <- rbind(grid,cbind(x = rep(12,100), y = seq(0,12,length.out = 100)))
grid <- rbind(grid,cbind(x =  seq(12,0,length.out = 100), y = rep(12,100)))
grid <- rbind(grid,cbind(x =  rep(0,100), y =seq(12,0,length.out = 100)))
grid <- cbind(grid , group = rep(6,400))
grid <- as.data.frame(grid)
dat <- data.frame(x = X[,1],y = X[,2],v = v2,s = s_01)
ggplot()+
  geom_voronoi(data = dat[dat$s ==1,],aes(x = x,y = y,fill = v),outline = grid ,size = 0.1,alpha = 1,colour ="black")+
  geom_point(data = dat,aes(x = x,y = y),shape = 1,size = 1,alpha = 0.5)+
  geom_point(data = dat[dat$s ==1,],aes(x = x,y = y),shape = 16,size = 1,alpha = 1)

