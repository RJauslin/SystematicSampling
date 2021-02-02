context("Systest")

test_that("test 2D grid",{
  
  skip("work in progress")
  rm(list = ls())
  set.seed(6)
  eps <- 1e-13
  N <- 144
  n <-  24
  pik <- rep(n/N,N)


  tb <- runif(2)/100

  X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
  tore = TRUE
  toreBound = sqrt(N)

  s <- Systest4(X,pik,tb,tore,toreBound,comment = TRUE)
  # plot(X)
  # points(X[s,1],X[s,2],pch = 16)
  #
  # 
  rm(list = ls())
  set.seed(6)
  eps <- 1e-13
  N <- 36
  n <-  6
  pik <- rep(n/N,N)
  s <- 16
  pik_cutted <- runif(1,min = eps,max = pik[s[1]] - eps)

  tb <- runif(2)/100
  X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
  tore = TRUE
  toreBound = sqrt(N)

  s <- Systest4(X,pik,tb,tore,toreBound,comment = TRUE,s = s,pik_cutted = pik_cutted)
  plot(X)
  points(X[s,1],X[s,2],pch = 16)

  # 
  
})



test_that("test 2D grid - missing strata",{
 
  skip("in progress")
   rm(list = ls())
  # set.seed(2)
  eps <- 1e-13
  N <- 36
  n <- 8
  pik <- rep(n/N,N)


  tb <- runif(2)/100
  X <- as.matrix(cbind(runif(N),runif(N)))
  tore = FALSE
  toreBound = FALSE

  s <- Systest4(X,pik,tb,comment = TRUE)
  
  
  # set.seed(10)
  # eps <- 1e-13
  # N <- 144
  # n <-  24
  # pik <- rep(n/N,N)
  # 
  # 
  # tb <- runif(2)/100
  # 
  # X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
  # tore = TRUE
  # toreBound = sqrt(N)
  # 
  # load("C:/Users/jauslinr/switchdrive/SysSampling/SysSampling/tests/testthat/envir1.RData")
  # 
  
  
})

