context("Strata number")


test_that("test 0  --- strat that cover two cutted units",{

  # add that i the

  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 8
  n <- 3
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  tb = -0.0001
  pik <- c(0.20000000, 0.30000000, 0.40000000, 0.70000000, 0.03741482, 0.00000000, 0.00000000, 0.36258518)
  pikInit <- pik
  # pikInit <- c(0.00000000, 0.07101996, 0.40000000, 0.70000000, 0.60000000, 0.20000000, 0, 0.00000000)
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##              reference unit and starting point               -
  ##---------------------------------------------------------------
  ref = 5
  k = 8
  ##----------------------------------------------------------------
  ##                      distance calculation                     -
  ##----------------------------------------------------------------
  d_ref <- distRef(X,ref,tb,tore,toreBound )
  d <- distRef(X,k,tb,tore,toreBound )



  ##----------------------------------------------------------------
  ##                              Test                             -
  ##----------------------------------------------------------------
  # (X,d,d_ref,pik,ref,k,pikInit = pikInit )
  # strataCompleteLink(X,pik,ref,ref,tb,pikInit,bound = 1.0,tore = FALSE,toreBound = -1)

})



test_that("test 1  --- Strata does not contain k anymore at the end",{


  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 8
  n <- 3
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  tb = 0.0001
  pik <- c(0.00000000, 0.07101996, 0.40000000, 0.70000000, 0.60000000, 0.20000000, 0.02898004, 0.00000000)
  # pik <- c(0.00000000, 0.07101996, 0.40000000, 0.70000000, 0.60000000, 0.20000000, 0, 0.00000000)
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##              reference unit and starting point               -
  ##---------------------------------------------------------------
  ref = 2
  k = 7
  ##----------------------------------------------------------------
  ##                      distance calculation                     -
  ##----------------------------------------------------------------
  d_ref <- distRef(X,ref,tb,tore,toreBound )
  d <- distRef(X,k,tb,tore,toreBound )



  ##----------------------------------------------------------------
  ##                              Test                             -
  ##----------------------------------------------------------------
  # expect_equal(strataUnitk(d,d_ref,pik,ref,k,pikInit = pik),NULL)
  expect_equal(strataCompleteLink(X,pik,ref,k,tb,pikInit = pik,tore = tore,toreBound = toreBound),NULL)

})


test_that("test 2 --- Strata NULL when ref is not inside strata",{


  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 36
  n <- 8
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  tb = -0.001
  pik <- rep(n/N,N)
  pikInit <- pik
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##              reference unit and starting point               -
  ##---------------------------------------------------------------
  ref = 5
  ##----------------------------------------------------------------
  ##                      distance calculation                     -
  ##----------------------------------------------------------------
  d_ref <- distRef(X,ref,tb)




  ##----------------------------------------------------------------
  ##                              Test                             -
  ##----------------------------------------------------------------
  # k = 9
  # for(k in 9:N){
  #   d <- distRef(X,k,tb)
  #   expect_equal(strataCompleteLink(X,pik,ref,k,tb,pikInit,bound = 1.0,tore = FALSE,toreBound = -1),NULL)
  #   # expect_equal(strataComplete(X,pik,ref,k,tb,pikInit = pik,tore = tore,toreBound = toreBound),NULL)
  # }

})

test_that("test 3 -- Strata NULL when ref is not inside strata",{

  rm(list = ls())
  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 8
  n <- 3
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------
  tb = 0.001
  pik <-  c(0.2000000, 0.3000000, 0.4000000, 0.7000000, 0.5625852, 0.2000000, 0.1000000, 0.5000000)
  tore = TRUE
  toreBound = N
  ##---------------------------------------------------------------
  ##              reference unit and starting point               -
  ##---------------------------------------------------------------
  k = 8
  ref = 5
  ##----------------------------------------------------------------
  ##                      distance calculation                     -
  ##----------------------------------------------------------------
  d <- distRef(X,k,tb)
  d_ref <- distRef(X,ref,tb)




  ##----------------------------------------------------------------
  ##                              Test                             -
  ##----------------------------------------------------------------
  # strata <- strataUnitk(d,d_ref,pik,ref,k,pikInit = pik)
  strata2 <- strataCompleteLink(X,pik,ref,5,tb,pikInit = pik,tore = tore,toreBound = toreBound)

  # expect_equal(sum(strata$w),1)
  expect_equal(sum(strata2$w),1)

  # expect_equal(strata$strata,c(5,6,7,8))
  expect_equal(strata2$strata,c(5,6,7,8))

  # expect_equal(sort(strata$strata_wo_modif),c(6,7))
  expect_equal(sort(strata2$strata_wo_modif),c(6,7))


  expect_equal(strata2$w[8],0.1374148)
  # expect_equal(strata$w[8],0.1374148)


  # expect_equal(strata$modif,8)
  expect_equal(strata2$modif,8)


})



test_that("test 4 --- tb have impact",{


  rm(list = ls())

  ##----------------------------------------------------------------
  ##                      Size and coordinates                     -
  ##----------------------------------------------------------------
  N <- 10
  n <- 3
  X <- as.matrix(seq(1,N,1))
  ##----------------------------------------------------------------
  ##            Inclusion probabilities and tore options           -
  ##----------------------------------------------------------------

  tb = -0.001
  pik <- rep(0.3,N)
  shift = TRUE
  tore = TRUE
  toreBound = 10
  ##---------------------------------------------------------------
  ##              reference unit and starting point               -
  ##---------------------------------------------------------------
  ref <- 6
  k = 4
  pik[ref] = 0.124897
  ##----------------------------------------------------------------
  ##                      distance calculation                     -
  ##----------------------------------------------------------------
  d <- distRef(X,k,tb)
  d_ref <- distRef(X,ref,tb)


  ##----------------------------------------------------------------
  ##                              Test                             -
  ##----------------------------------------------------------------
  expect_equal(strataCompleteLink(X,pik,ref,3,tb,pikInit = pik,tore = tore,toreBound = toreBound),NULL)
  tb = 0.001
  d <- distRef(X,k,tb)
  d_ref <- distRef(X,ref,tb)
  expect_s3_class(strataCompleteLink(X,pik,ref,ref,tb,pikInit = pik,tore = tore,toreBound = toreBound),"strata")

  # strataComplete(X,pik,ref,ref,-tb,pikInit = pik,tore = tore,toreBound = toreBound)
  # strataComplete(X,pik,ref,ref,tb,pikInit = pik,tore = tore,toreBound = toreBound)

})



