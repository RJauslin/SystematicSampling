#' Title
#'
#' La fonction va boucler sur les unités les plus proche pour trouver la strate tel que l'unité k est au bord
#' (coupé de la bonne manière) et un autre unité qui laisse le plus en dehors.
#'
#' @param X sptaial coordinates
#' @param pik_tmp temporary pik (unit k has a cutted pik)
#' @param tb
#' @param ref
#' @param bound
#' @param tore
#' @param shift
#' @param toreBound
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
#' ############################# 1D
#'
#' rm(list = ls())
#' eps <- 1e-13
#' N <- 36
#' n <- 8
#' X <- as.matrix(seq(1,N,1))
#' tb = -0.001
#' pik <- rep(n/N,N)
#' shift = TRUE
#' tore = TRUE
#' toreBound = N
#' comment = TRUE
#'
#'
#' s <- systematic(X,pik,tb,tore,toreBound,comment = TRUE)
#' plot(X,rep(0,N))
#' points(X[s],rep(0,n),pch = 16)
#'
#' ############################# 2D GRID UNEQUAL
#'
#'
#' rm(list = ls())
#' #set.seed(2)
#' # set.seed(8)
#' # set.seed(3)
#'  #set.seed(5) # n = 12
#' set.seed(7)
#' eps <- 1e-13
#' N <- 36
#' n <-  6
#' pik <- rep(n/N,N)
#' pik <- sampling::inclusionprobabilities(runif(N),n)
#'
#' tb <- runif(2)/100
#'
#' X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
#' tore = TRUE
#' toreBound = sqrt(N)
#' comment = TRUE
#'
#' s <- systematic(X,pik,tb,tore = tore,toreBound = toreBound,comment = comment)
#'
#' ############################# 2D GRID
#'
#'
#' rm(list = ls())
#' #set.seed(2)
#' # set.seed(8)
#' # set.seed(3)
#'  #set.seed(5) # n = 12
#' set.seed(7)
#' eps <- 1e-13
#' N <- 144
#' n <-  24
#' pik <- rep(n/N,N)
#'
#'
#' tb <- runif(2)/100
#'
#' X <- as.matrix(expand.grid(seq(1,sqrt(N),1),seq(1,sqrt(N),1)))
#' tore = TRUE
#' toreBound = sqrt(N)
#' comment = TRUE
#'
#' s <- systematic(X,pik,tb,tore = tore,toreBound = toreBound,comment = comment)
#'
#' plot(X)
#' points(X[s,],pch = 16)
#'
#'
#' ############################# 2D
#'
#'
#' rm(list = ls())
#' #set.seed(3)
#' eps <- 1e-13
#' N <- 100
#' n <- 10
#' pik <- rep(n/N,N)
#'
#'
#' tb <- rep(0,2)
#' X <- as.matrix(cbind(runif(N, min = 1,max = 3),runif(N,min = 1,max = 3)))
#' tore = FALSE
#' toreBound = -1
#' comment = TRUE
#' s <- systematic(X,pik,tb,tore = tore,toreBound = toreBound,comment = TRUE)
#' plot(X)
#' points(X[s,],pch = 16)
#'
#' s <- lpm1(pik,X)
#' plot(X)
#' points(X[s,],pch = 16)
#'
systematic <- function(X,
                     pik,
                     tb,
                     tore = FALSE,
                     toreBound = -1.0,
                     comment = FALSE,
                     pik_cutted,
                     s){

  ##################################################################
  ##                        Initialization                        ##
  ##################################################################
  pikInit <- pik

  N <- nrow(X)
  eps <- 1e-9
  p <- ggplot2::ggplot()

  pikstar <- pik


  ##---------------------------------------------------------------
  ##                To conditional inclusion prob                 -
  ##---------------------------------------------------------------

  if(missing(s)){
    cumulative <- c(0,cumsum(pik))
    u <- runif(1,0,sum(pik))
    s <- c()
    s[1] <- max(which(u-cumulative > eps))
  }


  # pikstar sera toujours pik avec une seule unité modifié ainsi les strates seront toujours pareil et c'est le vecteur tagged qui décidera quelle strates enlever.

  if(missing(pik_cutted)){
    pikstar[s[1]] <- runif(1,min = eps,max = pik[s[1]] - eps)
    # pik_s <-  runif(1,min = eps,max = pik[s[1]] - eps)
  }else{
    pikstar[s[1]] <- pik_cutted
    # pik_s <- pik_cutted
  }




  ##----------------------------------------------------------------
  ##                            For 1D                             -
  ##----------------------------------------------------------------

  # choosed_dir = FALSE
  # if(runif(1) < 1/2){
    # choosed_dir = FALSE
  # }


  ##---------------------------------------------------------------
  ##                          tagged unit                         -
  ##---------------------------------------------------------------

  tagged <- c()

  #################################################################
  ##                          Main Loop                          ##
  #################################################################




  #################################################################
  ##                          Main Loop                          ##
  #################################################################
  while(length(s) < sum(pik)){
    # while(length(s) < 11){


    ##-----1-----------------------------------------------------------

    pik_s <- pikstar[s[which(pikstar[s] > eps)]]
    s_tmp <- s[which(pikstar[s] > eps)]
    u_Strata <- list()
    for(k in 1:length(s_tmp)){
      l_Strata <- list()

      pik_tmp <- pikInit
      pik_tmp[s_tmp[k]] <- pik_s[k]

      ### REMOVE STRATING POINT OF s BUT WITHOUT putting pikstar == 0
      # tmp <- pikstar
      # pikstar[s_tmp] <- 0
      # pikstar[s_tmp[k]] <- tmp[s_tmp[k]]

      l_Strata <- allStrata(X,pik = pik_tmp,ref = s_tmp[k],
                            tb = tb,
                            pikInit = pikInit,
                            tore = tore,
                            toreBound = toreBound)

      # remove strata that contains tagged units
      # l_Strata <- l_Strata[!as.vector(do.call(cbind,lapply(l_Strata,function(x){any(tagged %in% x$strata)})))]
      class(l_Strata) <- c("l_strata")
      # plot(l_Strata,X)
      u_Strata <- c(u_Strata,unionStrata(l_Strata,s_tmp[k]))
      class(u_Strata) <- c("u_strata","l_strata")
      # plot.l_strata(u_Strata,X)




      # CHOOSE DIRECTION 1D
      # if(length(s)== 1 && ncol(X) == 1){
      #   u_Strata_tmp <- list()
      #   for(lll in 1:length(u_Strata)){
      #     if(u_Strata[[lll]]$direction == choosed_dir){
      #       u_Strata_tmp <- c(u_Strata_tmp,u_Strata[lll])
      #     }
      #   }
      #   u_Strata <- u_Strata_tmp
      # }

      # pikstar <- tmp
    }




    # FOR 1D principally (firstly we do the union of the all strata ) and after we remove those wo are tagged ..... not sure if we need both
    # u_Strata <- unionStrata(u_Strata,s_tmp)

    u_Strata <- u_Strata[!as.vector(do.call(cbind,lapply(u_Strata,function(x){any(tagged %in% x$strata)})))]
    class(u_Strata) <- c("u_strata","l_strata")
    # plot.l_strata(u_Strata,X)

    # les deux unité coupé sont dans la strate -> FALSE
    # TRUE Si l'une des deux unité coupé est modif
    if(length(s_tmp) > 1.1){ # ofc si il y a au moins deux unités ;-)
      bothunit <- as.vector(do.call(cbind,lapply(u_Strata,
                                                 FUN = function(x){
                                                   if(all(s_tmp %in% x$strata)){
                                                     return(any(s_tmp %in% x$modif))
                                                   }else{
                                                     return(TRUE)
                                                   }
                                                 })))

      u_Strata <- u_Strata[bothunit]
    }




    #
    #
    #
    ## Si il n'existe pas de strates alors on recalcul avec les probas égale à 0
    #
    #
    #

    if(length(u_Strata) == 0){
      u_Strata <- list()
      for(k in 1:length(s_tmp)){
        l_Strata <- list()

        # pik_tmp <- pikInit
        # pik_tmp[s_tmp[k]] <- pik_s[k]

        ### REMOVE STRATING POINT OF s BUT WITHOUT putting pikstar == 0
        tmp <- pikstar
        pikstar[s_tmp] <- 0
        pikstar[s_tmp[k]] <- tmp[s_tmp[k]]

        l_Strata <- allStrata(X,pik = pikstar,ref = s_tmp[k],
                              tb = tb,
                              pikInit = pikInit,
                              tore = tore,
                              toreBound = toreBound)

        # remove strata that contains tagged units
        # l_Strata <- l_Strata[!as.vector(do.call(cbind,lapply(l_Strata,function(x){any(tagged %in% x$strata)})))]
        class(l_Strata) <- c("l_strata")
        # plot(l_Strata,X)
        u_Strata <- c(u_Strata,unionStrata(l_Strata,s_tmp[k]))
        class(u_Strata) <- c("u_strata","l_strata")
        # plot.l_strata(u_Strata,X)
        # if(length(s)== 1 && ncol(X) == 1){
        #   u_Strata_tmp <- list()
        #   for(lll in 1:length(u_Strata)){
        #     if(u_Strata[[lll]]$direction == choosed_dir){
        #       u_Strata_tmp <- c(u_Strata_tmp,u_Strata[lll])
        #     }
        #   }
        #   u_Strata <- u_Strata_tmp
        # }

        pikstar <- tmp
      }

      u_Strata <- u_Strata[!as.vector(do.call(cbind,lapply(u_Strata,function(x){any(tagged %in% x$strata)})))]
    }


    u_d_ref_modif <- round(do.call(rbind,lapply(u_Strata,FUN = function(x){return(x$d_ref_modif)})),10)

    # u_Strata peut avoir la même strate plusieurs fois mais avec un d_ref_modif différent. -> il faut clean et garder que celle qui a la plus courte.

    if(any(duplicated(u_d_ref_modif))){
      mtmp <- which_min(u_d_ref_modif)
      u_d_ref_modif <- u_d_ref_modif[mtmp]
      u_Strata <- u_Strata[mtmp]
    }


    # u_Strata
    if(length(unique(u_d_ref_modif)) == 1 & length(u_Strata) > 1.1){
      for(iiii in 1:length(u_Strata)){
        d_ref <- distRef(X,u_Strata[[iiii]]$ref,tb,tore,toreBound)
        u_Strata[[iiii]]$d_ref_modif <- d_ref[u_Strata[[iiii]]$modif]
      }
    }
    class(u_Strata) <- c("u_strata","l_strata")
    # u_Strata <- unionStrata(u_Strata,s_tmp)
    plot(u_Strata,X)
    ##----------------------------------------------------------------

      final <- goodStrata(u_Strata)




    ##---------------------------------------------------------------
    ##                          tagged unit                         -
    ##---------------------------------------------------------------

    tagged <- c(tagged,final$strata_wo_modif)




    ##################################################################
    ##                     Update pikstar and s                     ##
    ##################################################################






    if(length(s_tmp) == 1){
      pikstar[s_tmp] <- pikInit[s_tmp]
    }
    st <- final$strata
    for(uu in 1:length(st)){
      pikstar[st[uu]] <- pikstar[st[uu]] - final$w[st[uu]]
    }



    # pikstar[final$modif] <- pik[final$modif] - final$w[final$modif]
    # pikstar[final$ref] <- pik[final$ref] - final$w[final$modif]



    ##----------------------------------------------------------------
    ##                              Plot                             -
    ##----------------------------------------------------------------
    if(comment == TRUE){
      if(ncol(X) == 1){
        p_tmp <- plot(final,X)
        # p <- p + p_tmp
        # p <- plot(final,X)
        print(p_tmp)

        # points(X[which(pikstar < eps)],rep(0,length(which(pikstar < eps))),pch = 16,col = "red")
        # points(X[s],rep(0,length(s)),pch = 16,col = "black")
        # print(final$w);cat("\n\n\n\n")
        # Sys.sleep(3)
      }else{

        X_dat <- as.data.frame(X)
        colnames(X_dat) <- c("x","y")

        X_chull <- as.data.frame(X_dat[final$strata,])
        X_chull <- X_chull[chull(X_chull),]

        # p <- p +
        #   geom_point(data = X_dat,aes(x = x,y = y),shape = 1,alpha = 0.4)+
        #   geom_polygon(data = X_chull,aes(x = x,y = y),fill = "red",colour = "black",alpha = 0.4)+
        #   geom_point(data = X_dat[final$modif,],aes(x = x,y = y),pch = 16,colour = "grey50")+
        #   geom_point(data = X_dat[s,],aes(x = x,y = y),pch = 16,colour = "black")+
        #   geom_point(data = X_dat[s[which(pikstar[s] > eps)],],aes(x = x,y = y),pch = 16,colour = "darkturquoise",size = 2)+
        #   theme_void()
        # print(p)


        plot(final,X)
        points(X[which(pikstar < eps),1],X[which(pikstar < eps),2],pch = 16,col = "red")
        points(X[s,1],X[s,2],pch = 16,col = "black")
        points(X[s[which(pikstar[s] > eps)],1],X[s[which(pikstar[s] > eps)],2],pch = 16,col = "darkturquoise")
        Sys.sleep(5)
      }

    }

    ##################################################################
    ##                     restart                                  ##
    ##################################################################

    if(sum(pikstar[s]) < eps){


      d_ref <- distRef(X,final$ref,tb = tb,tore,toreBound)
      d_ref[final$modif] <- NA
      d_ref <- d_ref[final$strata]
      d_modif <- distRef(X,final$modif,tb = tb,tore,toreBound)
      d_modif[final$ref] <- NA
      d_modif <- d_modif[final$strata]


      test <- final$strata[which.max(d_ref + d_modif)]

      # points(X[test,1],X[test,2],col = "cyan",pch = 16)

      d_restart <-  distRef(X,test,tb = rep(0,ncol(X)),tore,toreBound)
      d_restart[tagged] <- 1e9
      d_restart[s] <- 1e9
      rst <- which_min(d_restart)

      if(length(rst) > 1){
        d_tmp1 <-  distRef(X,final$ref,tb = tb,tore,toreBound)
        d_tmp1[tagged] <- 1e9
        d_tmp1[s] <- 1e9
        d_tmp2 <-  distRef(X,final$modif,tb = tb,tore,toreBound)
        d_tmp2[tagged] <- 1e9
        d_tmp2[s] <- 1e9
        d_tmp <- d_tmp1 + d_tmp2
        rst <- rst[which.max(d_tmp[rst])]
      }



      s <- c(s,rst)
      pikstar[s[length(s)]] <- runif(1,min = eps,max = pikstar[s[length(s)]] - eps)


      # points(X[rst,1],X[rst,2],col = "pink",pch = 16)



      # break;
    }else{
      s <- c(s,final$modif)
    }



    ##----------------------------------------------------------------
    ##                      add final to modif                       -
    ##----------------------------------------------------------------



    # print(final)


    # print(pikstar[s])
    # print(length(s))
  }



  return(s)



}

