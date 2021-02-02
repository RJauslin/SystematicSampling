context("Strata number")



test_that("test 3 1D k = 38",{


  skip("in progress")
  # rm(list = ls())
  # ##----------------------------------------------------------------
  # ##                      Size and coordinates                     -
  # ##----------------------------------------------------------------
  # N <- 8
  # X <- as.matrix(seq(1,N,1))
  # ##----------------------------------------------------------------
  # ##            Inclusion probabilities and tore options           -
  # ##----------------------------------------------------------------
  # pik <- c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
  # tore = TRUE
  # toreBound = N
  # ##---------------------------------------------------------------
  # ##                     systematic sampling                      -
  # ##---------------------------------------------------------------
  # mat <- systematicDesign(pik)$samples
  # set.seed(38)
  # tb = 0.0001
  #
  #
  # pikInit <- pik
  #
  # N <- nrow(X)
  # eps <- 1e-9
  # p <- ggplot2::ggplot()
  #
  # pikstar <- pik
  # cumulative <- c(0,cumsum(pik))
  # u <- runif(1,0,sum(pik))
  # s <- c()
  # s[1] <- max(which(u-cumulative > eps))
  #
  # pikstar[s[1]] <- runif(1,min = eps,max = pik[s[1]] - eps)
  #
  #
  # l_Strata <- allStrata(X,pik = pikstar,ref = s[1],
  #           tb = tb,
  #           pikInit = pikInit,
  #           tore = tore,
  #           toreBound = toreBound,
  #           s_omit = NULL)
  # l_StrataComplete <- allStrataComplete(X,pik = pikstar,ref = s[1],
  #                       tb = tb,
  #                       pikInit = pikInit,
  #                       tore = tore,
  #                       toreBound = toreBound,
  #                       s_omit = NULL)
  #
  #
  # pikInit
  # pik <- pikstar
  # ref = s[1]
  # k = ref
  # bound = 1.0
  # strataComplete(X,pik,ref = s[1],k = 1,tb,pikInit,tore = tore,toreBound = toreBound)
  #
  # l_Strata
  # l_StrataComplete
  #
  #
  # s <- Systest5(X,pik,tb = tb,tore = tore,toreBound = toreBound)
  # s_01 <- rep(0,N)
  # s_01[s] <- 1
  # any(apply(mat,1,compare,v = s_01))
  # # print(s)
  # if(any(apply(mat,1,compare,v = s_01))!= TRUE){
  #   print(k)
  # }
  # expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)
  #
  #
})
