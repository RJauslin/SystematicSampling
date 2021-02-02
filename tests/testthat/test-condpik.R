context("test conditional inclusion probabilities")

test_that("Unequal on unit 4",{
  skip("Too long (with simulation).")

  ##----------------------------------------------------------------
  ##          True value of the conditional inclusion prob         -
  ##----------------------------------------------------------------
  rm(list = ls())

  N <- 8
  eps <- 1e-9
  X <- as.matrix(seq(1,N,1))
  pik=c(0.2,0.3,0.4,0.7,0.6,0.2,0.1,0.5)
  mat <- systematicDesign(pik)$samples

  PI=UPsystematicpi2(pik)
  j = 4
  PI[j,]/PI[j,j]



  r=systematicDesign(pik)
  SS=r$samples
  p=r$p

  PP=array(0,c(N,N))
  for(k in 1:N)
  {
    TEST=SS[,k]==1
    PP[k,]=c(t(matrix(SS[TEST,],ncol=N))%*%p[TEST])/pik[k]
  }



  ##----------------------------------------------------------------
  ##                          Simulation                           -
  ##----------------------------------------------------------------


  ref = 4

  tore = TRUE
  shift = TRUE
  toreBound = 8

  SIM <-  1000
  s <- matrix(rep(0,8*SIM),ncol = SIM,nrow = 8)
  for(i in 1:SIM){
    set.seed(i)
    print(i)
    pik_cutted <- runif(1,min = eps,max = pik[ref] - eps)
    # pik_cutted <- c(pik_cutted,pik[ref] - pik_cutted)
    tb <- runif(1,min = 0,max = 0.001)
    s_tmp <- systematic(X,pik,tb,tore = TRUE,toreBound = toreBound,
                      comment = FALSE,
                      pik_cutted = pik_cutted,
                      s = ref)



    s_01 <- rep(0,N)
    s_01[s_tmp] <- 1

    expect_equal(any(apply(mat,1,compare,v = s_01)),TRUE)
    p <- c(p,which(apply(mat,1,compare,v = s_01)))
    s[,i] <- s_01
  }

  pike=rowSums(s)/SIM
  pike
  PP[ref,]

  # t-test
  expect_equal(all(abs((pike - PP[ref,])/sqrt(PP[ref,]*(1-PP[ref,])/SIM)) < 2,na.rm = TRUE),TRUE)

})
