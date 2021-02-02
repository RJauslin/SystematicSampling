context("test on inclusion probabilities")


test_that("1D uneqal N = 8 --- Verifiy that inclusion probabilities are correct",{

  skip("Too long")
  rm(list = ls())

  SIM <-  1000
  s <- matrix(rep(0,3*SIM),ncol = 3,nrow = SIM)
  p <- p2 <- c()
  for(i in 1:SIM){
    print(i)
    set.seed(i)
    N <- 8
    X <- as.matrix(seq(1,N,1))
    pik <- c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
    tb = 0.001
    mat <- systematicDesign(pik)$samples
    tore = TRUE
    shift = TRUE
    toreBound = 8


    s[i,] <- systematic(X,pik = pik,tb = tb,tore = TRUE,toreBound = toreBound)


    s_01 <- rep(0,N)
    s_01[s[i,]] <- 1
    p2 <- c(p2,which(apply(mat,1,compare,v = UPsystematic(pik))))
    p <- c(p,which(apply(mat,1,compare,v = s_01)))

    expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)
  }

  p_hat2 <- table(p2)/SIM
  p_hat <- table(p)/SIM
  p <- as.vector(systematicDesign(pik)$p)


  expect_equal(all(abs( (p_hat - p)/sqrt(p*(1-p)/SIM)) < 2,na.rm = TRUE),TRUE)
  expect_equal(all(abs( (p_hat2 - p)/sqrt(p*(1-p)/SIM)) < 2,na.rm = TRUE),TRUE)
})



test_that("1D uneqal N = 30 --- Verifiy that inclusion probabilities are correct",{

  skip("Too long")

  SIM <-  1000
  N <- 30
  n <- 8
  X <- as.matrix(seq(1,N,1))
  pik <- sampling::inclusionprobabilities(runif(N),n)
  tb = 0.001
  tore = TRUE
  shift = TRUE
  toreBound = N
  mat <- systematicDesign(pik)$samples

  s <- matrix(rep(0,n*SIM),ncol = n,nrow = SIM)
  p <- p2 <- c()
  for(i in 1:SIM){
    print(i)
    set.seed(i)


    s[i,] <- systematic(X,pik = pik,tb = tb,tore = TRUE,toreBound = toreBound)
    # s[i,] <- Systest3(X,pik = pik,tb = tb,tore = TRUE,toreBound = toreBound)
    # s[i,] <- Systest3(X,pik = pik,tb = tb,tore = TRUE,toreBound = toreBound)

    # test <- try({s[i,] <- Systest3(X,pik = pik,tb = tb,tore = TRUE,toreBound = toreBound)})
    # if(class(test)=="try-error"){
    #   print(i)
    #   break;
    # }

    s_01 <- rep(0,N)
    s_01[s[i,]] <- 1
    p2 <- c(p2,which(apply(mat,1,compare,v = UPsystematic(pik))))
    p <- c(p,which(apply(mat,1,compare,v = s_01)))

    expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)
  }

  p_UPsys <- table(p2)/SIM
  p_Systest <- table(p)/SIM
  p_true <- as.vector(systematicDesign(pik)$p)

  cbind(p_UPsys,p_Systest,p_true)

  expect_equal(all(abs( (p_UPsys - p_true)/sqrt(p_true*(1-p_true)/SIM)) < 2,na.rm = TRUE),TRUE)
  expect_equal(all(abs( (p_Systest - p_true)/sqrt(p_true*(1-p_true)/SIM)) < 2,na.rm = TRUE),TRUE)
})



test_that("2D uneqal N = 144 --- Verifiy that inclusion probabilities are correct",{

  skip("Too long")


})
