context("Systest")

test_that("test 1 1D unequal (related to strataUnit see test 1) --- Always a systematic sampling",{

  #############
  # SOLVED BY REMOVING THAT other s units could by used by the starta


  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 8
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  pik <- c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##                     systematic sampling                      -
  ##---------------------------------------------------------------
  mat <- systematicDesign(pik)$samples

  SIM <- 50

  set.seed(1)
  tb = 0.0001
  s <- systematic(X,pik,tb = tb,tore = tore,toreBound = toreBound)
  s_01 <- rep(0,N)
  s_01[s] <- 1
  print(s)
  expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)


})




test_that("test 2 1D unequal (related to strataUnit see test 1) --- Always a systematic sampling",{

  #############
  # SOLVED BY REMOVING THAT other s units could by used by the starta


  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 8
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  pik <- c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##                     systematic sampling                      -
  ##---------------------------------------------------------------
  mat <- systematicDesign(pik)$samples

  SIM <- 50

  set.seed(13)
  tb = 0.0001
  s <- systematic(X,pik,tb = tb,tore = tore,toreBound = toreBound)
  s_01 <- rep(0,N)
  s_01[s] <- 1
  print(s)
  expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)


})


test_that("test 3 1D unequal (related to strataUnit see test 1) --- Always a systematic sampling",{

  #############
  # SOLVED BY REMOVING THAT other s units could by used by the starta


  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 8
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  pik <- c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##                     systematic sampling                      -
  ##---------------------------------------------------------------
  mat <- systematicDesign(pik)$samples

  SIM <- 50

  set.seed(18)
  tb = 0.0001
  s <- systematic(X,pik,tb = tb,tore = tore,toreBound = toreBound)
  s_01 <- rep(0,N)
  s_01[s] <- 1
  print(s)
  expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)


})

test_that("test 1D unequal (related to strataUnit see test 1) --- Always a systematic sampling",{

  #############
  # SOLVED BY REMOVING THAT other s units could by used by the starta


  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 8
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  pik <- c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##                     systematic sampling                      -
  ##---------------------------------------------------------------
  mat <- systematicDesign(pik)$samples


  set.seed(6)
  tb = 0.0001
  s <- systematic(X,pik,tb = tb,tore = tore,toreBound = toreBound)
  s_01 <- rep(0,N)
  s_01[s] <- 1
  print(s)
  expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)


})



test_that("test 3 1D k = 38",{

  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 8
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  pik <- c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##                     systematic sampling                      -
  ##---------------------------------------------------------------
  mat <- systematicDesign(pik)$samples
  set.seed(38)
  tb = 0.0001
  s <- systematic(X,pik,tb = tb,tore = tore,toreBound = toreBound)
  s_01 <- rep(0,N)
  s_01[s] <- 1

  expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)


})



test_that("test 3 1D unequal --- Always a systematic sampling",{

  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 8
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  pik <- c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##                     systematic sampling                      -
  ##---------------------------------------------------------------
  mat <- systematicDesign(pik)$samples

  SIM <- 50
  for(k in 1:SIM){
    # print(k)
    set.seed(k)
    tb = 0.0001
    s <- systematic(X,pik,tb = tb,tore = tore,toreBound = toreBound)
    s_01 <- rep(0,N)
    s_01[s] <- 1
    # print(s)
    if(any(apply(mat,1,compare,v = s_01))!= TRUE){
      print(k)
    }
    expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)
  }

})


test_that("test 4 1D unequal (related to strataUnit see test 1) --- Always a systematic sampling",{

  rm(list = ls())

  N <- 8
  eps <- 1e-9
  X <- as.matrix(seq(1,N,1))
  pik=c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
  mat <- systematicDesign(pik)$samples


  ref = 4

  tore = TRUE
  shift = TRUE
  toreBound = 8
  comment = TRUE

  set.seed(6)
  pik_cutted <- runif(1,min = eps,max = pik[ref] - eps)
  tb <- runif(1,min = -0.001,max = 0.001)
  s = ref
  s <- systematic(X,pik,tb,tore = TRUE,toreBound = toreBound,
                comment = FALSE,
                pik_cutted = pik_cutted,
                s = ref)



  s_01 <- rep(0,N)
  s_01[s] <- 1
  expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)

})
