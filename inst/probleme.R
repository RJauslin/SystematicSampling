rm(list = ls())
#set.seed(2)
# set.seed(8)
# set.seed(3)
 #set.seed(5) # n = 12
set.seed(7)
multi = TRUE
bound = 1
eps <- 1e-13
N <- 36
n <-  6
pik <- rep(n/N,N)
pik <- sampling::inclusionprobabilities(runif(N),n)

tb <- runif(2)/100

X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
tore = TRUE
toreBound = sqrt(N)
comment = TRUE

pikInit <- pik

N <- nrow(X)
eps <- 1e-9
p <- ggplot2::ggplot()

pikstar <- pik


cumulative <- c(0,cumsum(pik))
u <- runif(1,0,sum(pik))
s <- c()
s[1] <- max(which(u-cumulative > eps))
ref = s
pikstar[s[1]] <- runif(1,min = eps,max = pik[s[1]] - eps)

tagged <- c()


pik_s <- pikstar[s[which(pikstar[s] > eps)]]
s_tmp <- s[which(pikstar[s] > eps)]
u_Strata <- list()
# for(k in 1:length(s_tmp)){
k = 1

  l_Strata <- list()

  pik_tmp <- pikInit
  pik_tmp[s_tmp[k]] <- pik_s[k]

  tbl <- combTb(tb)
  tt = 2
  k = 9
  strataCompleteLink(X = X,pik = pik,ref = ref,k = 9,tb = as.vector(as.numeric(tbl[tt,])),
                     pikInit,bound = bound,tore = tore,toreBound = toreBound)

  tb = as.vector(as.numeric(tbl[tt,]))



