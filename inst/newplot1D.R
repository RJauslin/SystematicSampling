

rm(list = ls())
eps <- 1e-13
N <- 10
n <- 3
X <- as.matrix(seq(1,N,1))
tb = runif(1,min = -0.1,max = 0.1)/100
pik <- rep(n/N,N)
shift = TRUE
tore = TRUE
toreBound = N
comment = TRUE




pikInit <- pik
cumulative <- c(0,cumsum(pik))
u <- runif(1,0,sum(pik))
s <- c()
s[1] <- max(which(u-cumulative > eps))


pik[s[1]] <- runif(1,min = eps,max = pik[s[1]] - eps)
Strata <- strataCompleteLink(X,pik,s[1],k = s[1],tb,pikInit,tore = TRUE,toreBound = 10)


p <- plot(Strata,X)
p <- addStrata(Strata,X,p)

tb
dat <- data.frame(x = X,
                  pik = Strata$pikInit,
                  cm = cumsum(Strata$pikInit))
dat_ref <-  data.frame(x = X[Strata$ref,],
                       cm = cumsum(Strata$pik)[Strata$ref])

if(tb < 1e-7){

  refp <- cumsum(Strata$pik[1:Strata$ref])[Strata$ref]
  if(refp-1 < 1e-7){
    dat_rect <- data.frame(xmin = c(0,sum(Strata$pikInit)-(1-refp)),
                           xmax = c(refp,sum(Strata$pikInit)),
                           ymin = c(-0.05,-0.05),
                           ymax = c(0.05,0.05))
  }else{
    dat_rect <- data.frame(xmin = refp-1,
                           xmax = refp,
                           ymin = -0.05,
                           ymax = 0.05)
  }
}else{
  refp <- cumsum(Strata$pik[1:Strata$ref])[Strata$ref]
  if(refp + 1 > sum(pikInit)-1e-7){
    dat_rect <- data.frame(xmin = c(refp,0),
                           xmax = c(sum(Strata$pikInit),1  + (refp - sum(Strata$pikInit))),
                           ymin = c(-0.05,-0.05),
                           ymax = c(0.05,0.05))
  }else{
    dat_rect <- data.frame(xmin = refp,
                           xmax = refp+1,
                           ymin = -0.05,
                           ymax = 0.05)
  }
}


ggplot()+
  geom_segment(data = dat,aes(x = 0, y = 0, xend = cm[10], yend = 0))+
  geom_point(data = dat,aes(x = cm,y = 0),pch = 124,size = 5)+
  geom_point(data = dat_ref,aes(x = cm,y = 0),pch = 124,size = 10,col = "blue")+
  geom_rect(data = dat_rect,aes(xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax),alpha = 0.2,colour = "red") + ylim(-1,1)




col <- rep("Units",length(Strata$strata))
col[which(Strata$strata == Strata$ref)] <- "ref"
col[which(Strata$strata == Strata$modif)] <- "modif"
dat_s <- data.frame(x = X[Strata$strata,],
                    pik = pik[Strata$strata],
                    w = Strata$w[Strata$strata],
                    order = Strata$strata,
                    col = col)
dat_s$col <- factor(dat_s$col)
if(Strata$direction == FALSE){
  dat_s <- dat_s[order(dat_s$order),]
}



p<- ggplot()+
  # geom_point(data = c_pik,aes(x = x,y = rep(0,nrow(X))),pch = 124,size = 5)+
  geom_rect(data = data,aes(xmin = x - pik/2,ymin = 0 - pik/2,xmax = x +pik/2,ymax = 0 +pik/2),
            alpha = 1, colour = "grey70",fill = "grey90")+
  geom_rect(data = dat_s[1,],aes(xmin = (x+pik/2)-w,ymin = 0 - pik/2,xmax = x + pik/2,ymax = 0 +pik/2,fill = col),
            alpha = 1, colour = "grey50")+
  geom_rect(data = dat_s[nrow(dat_s),],aes(xmin = (x-pik/2),ymin = 0 - pik/2,xmax = x - pik/2 + w,ymax = 0 +pik/2,fill = col),
            alpha = 1, colour = "grey50")+
  ylim(c(-1,1)) +
  theme_bw()

if(2 != nrow(dat_s)){p <- p +
  geom_rect(data = dat_s[2:(nrow(dat_s)-1),],aes(xmin = (x-pik/2),ymin = 0 - pik/2,xmax = x - pik/2 + w,ymax = 0 +pik/2,fill = col),
            alpha = 1, colour = "grey50" )
}
p
