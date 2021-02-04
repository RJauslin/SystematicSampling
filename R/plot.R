#' Title
#'
#' @param X
#' @param strata
#'
#' @return
#' @export
#'
#' @examples
plot.strata <- function(Strata,X){
  require(ggplot2)

  if(ncol(X) == 1){

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
      if(refp + 1 > sum(Strata$pikInit)-1e-7){
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


    p <- ggplot()+
      geom_segment(data = dat,aes(x = 0, y = 0, xend = cm[nrow(X)], yend = 0))+
      geom_point(data = dat,aes(x = cm,y = 0),pch = 124,size = 15)+
      geom_point(data = dat_ref,aes(x = cm,y = 0),pch = 124,size = 10,col = "blue")+
      geom_rect(data = dat_rect,aes(xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax),alpha = 0.2,colour = "red",fill = "red") + ylim(-1,1)
    p


#
#     data <- data.frame(x = X,
#                        pik = Strata$pikInit)
#
#
#     col <- rep("Units",length(Strata$strata))
#     col[which(Strata$strata == Strata$ref)] <- "ref"
#     col[which(Strata$strata == Strata$modif)] <- "modif"
#     dat_s <- data.frame(x = X[Strata$strata,],
#                         pik = pik[Strata$strata],
#                         w = Strata$w[Strata$strata],
#                         order = Strata$strata,
#                         col = col)
#     dat_s$col <- factor(dat_s$col)
#     if(Strata$direction == FALSE){
#       dat_s <- dat_s[order(dat_s$order),]
#     }
#
#
#
#     p<- ggplot()+
#         # geom_point(data = c_pik,aes(x = x,y = rep(0,nrow(X))),pch = 124,size = 5)+
#         geom_rect(data = data,aes(xmin = x - pik/2,ymin = 0 - pik/2,xmax = x +pik/2,ymax = 0 +pik/2),
#                   alpha = 1, colour = "grey70",fill = "grey90")+
#         geom_rect(data = dat_s[1,],aes(xmin = (x+pik/2)-w,ymin = 0 - pik/2,xmax = x + pik/2,ymax = 0 +pik/2,fill = col),
#                   alpha = 1, colour = "grey50")+
#         geom_rect(data = dat_s[nrow(dat_s),],aes(xmin = (x-pik/2),ymin = 0 - pik/2,xmax = x - pik/2 + w,ymax = 0 +pik/2,fill = col),
#                   alpha = 1, colour = "grey50")+
#         ylim(c(-1,1)) +
#         theme_bw()
#
#       if(2 != nrow(dat_s)){p <- p +
#           geom_rect(data = dat_s[2:(nrow(dat_s)-1),],aes(xmin = (x-pik/2),ymin = 0 - pik/2,xmax = x - pik/2 + w,ymax = 0 +pik/2,fill = col),
#                     alpha = 1, colour = "grey50" )
#       }
#     p



    #
    # plot(X[,1],rep(0,N))
    # points(X[Strata$strata,1],rep(0,length(Strata$strata)),pch = 16,col = "green")
    # points(X[Strata$modif,1],rep(0,length(Strata$modif)),pch = 16,col = "orange")
    # if(any(names(Strata) == "ref")){
    #   points(X[Strata$ref,1],rep(0,length(Strata$ref)),pch = 16,col = "black")
    #   legend("topleft", legend=c("modif", "ref"),
    #          col=c("orange", "black"),lty = c(1,1), cex=0.8)
    # }
    # if(any(names(Strata) == "k")){
    #   points(X[Strata$k,1],rep(0,length(Strata$k)),pch = 16,col = "blue")
    #   legend("topleft", legend=c("modif", "ref","k"),
    #          col=c("orange", "black","blue"),lty = c(1,1,1), cex=0.8)
    #
    # }

  }else{


    X_dat <- as.data.frame(X)
    colnames(X_dat) <- c("x","y")

    require(ggalt)
    p <- ggplot()
    colors <- c("ref" = "black", "modif" = "orange", "k" = "blue")

    p <- p +
      geom_point(data = X_dat,aes(x = x,y = y),shape = 1,alpha = 0.4)+
      geom_encircle(data=X_dat[Strata$strata,],aes(x=x,y=y), colour = "black", spread=0.04) +
      # geom_point(data = X_dat[l_strata[[i]]$strata,],aes(x = x,y = y),fill = i,colour = i,alpha = 0.4,pch = 16)+
      # geom_polygon(data = X_chull,aes(x = x,y = y),fill = i,colour = "black",alpha = 0.4)+
      geom_point(data = X_dat[Strata$ref,],aes(x = x,y = y, color = "ref"),shape = 16)+
      geom_point(data = X_dat[Strata$modif,],aes(x = x,y = y,color = "modif"),shape = 16)+
      geom_point(data = X_dat[Strata$k,],aes(x = x,y = y,color = "k"),shape = 16)+
      ggtitle("Strata")+
      scale_color_manual(values = colors)+
      theme_systematic()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "bottom")
    p


    # plot(X)
    # points(X[Strata$strata,1],X[Strata$strata,2],pch = 16,col = "green")
    # points(X[Strata$modif,1],X[Strata$modif,2],pch = 16,col = "orange")
    # if(any(names(Strata) == "ref")){
    #   points(X[Strata$ref,1],X[Strata$ref,2],pch = 16,col = "black")
    #   legend("topleft", legend=c("modif", "ref"),
    #          col=c("orange", "black"),pch = c(16,16), cex=0.8)
    # }
    # if(any(names(Strata) == "k")){
    #   points(X[Strata$k,1],X[Strata$k,2],pch = 16,col = "blue")
    #   legend("topleft", legend=c("modif","k"),
    #          col=c("orange","blue"), pch = c(16,16), cex=0.8)
    # }
    # if(any(names(Strata) == "k") & any(names(Strata) == "ref")){
    #
    #   points(X[Strata$k,1],X[Strata$k,2],pch = 16,col = "blue")
    #   points(X[Strata$ref,1],X[Strata$ref,2],pch = 16,col = "black")
    #
    #   legend("topleft", legend=c("modif","k","ref"),
    #          col=c("orange","blue","black"), pch = c(16,16,16), cex=0.8)
    # }
  }
  return(p)
}



#' Title
#'
#' @param Strata
#' @param X
#' @param p
#'
#' @return
#' @export
#'
#' @examples
addStrata <- function(Strata,X,p){

  require(ggplot2)

  if(ncol(X) == 1){

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
      if(refp + 1 > sum(Strata$pikInit)-1e-7){
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


    p <- p +
      # geom_segment(data = dat,aes(x = 0, y = 0, xend = cm[nrow(X)], yend = 0))+
      geom_point(data = dat,aes(x = cm,y = 0),pch = 124,size = 5)+
      geom_point(data = dat_ref,aes(x = cm,y = 0),pch = 124,size = 10,col = "blue")+
      geom_rect(data = dat_rect,aes(xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax),alpha = 0.2,colour = "red")



    #
    #     data <- data.frame(x = X,
    #                        pik = Strata$pikInit)
    #
    #
    #     col <- rep("Units",length(Strata$strata))
    #     col[which(Strata$strata == Strata$ref)] <- "ref"
    #     col[which(Strata$strata == Strata$modif)] <- "modif"
    #     dat_s <- data.frame(x = X[Strata$strata,],
    #                         pik = pik[Strata$strata],
    #                         w = Strata$w[Strata$strata],
    #                         order = Strata$strata,
    #                         col = col)
    #     dat_s$col <- factor(dat_s$col)
    #     if(Strata$direction == FALSE){
    #       dat_s <- dat_s[order(dat_s$order),]
    #     }
    #
    #
    #
    #     p<- ggplot()+
    #         # geom_point(data = c_pik,aes(x = x,y = rep(0,nrow(X))),pch = 124,size = 5)+
    #         geom_rect(data = data,aes(xmin = x - pik/2,ymin = 0 - pik/2,xmax = x +pik/2,ymax = 0 +pik/2),
    #                   alpha = 1, colour = "grey70",fill = "grey90")+
    #         geom_rect(data = dat_s[1,],aes(xmin = (x+pik/2)-w,ymin = 0 - pik/2,xmax = x + pik/2,ymax = 0 +pik/2,fill = col),
    #                   alpha = 1, colour = "grey50")+
    #         geom_rect(data = dat_s[nrow(dat_s),],aes(xmin = (x-pik/2),ymin = 0 - pik/2,xmax = x - pik/2 + w,ymax = 0 +pik/2,fill = col),
    #                   alpha = 1, colour = "grey50")+
    #         ylim(c(-1,1)) +
    #         theme_bw()
    #
    #       if(2 != nrow(dat_s)){p <- p +
    #           geom_rect(data = dat_s[2:(nrow(dat_s)-1),],aes(xmin = (x-pik/2),ymin = 0 - pik/2,xmax = x - pik/2 + w,ymax = 0 +pik/2,fill = col),
    #                     alpha = 1, colour = "grey50" )
    #       }
    #     p



    #
    # plot(X[,1],rep(0,N))
    # points(X[Strata$strata,1],rep(0,length(Strata$strata)),pch = 16,col = "green")
    # points(X[Strata$modif,1],rep(0,length(Strata$modif)),pch = 16,col = "orange")
    # if(any(names(Strata) == "ref")){
    #   points(X[Strata$ref,1],rep(0,length(Strata$ref)),pch = 16,col = "black")
    #   legend("topleft", legend=c("modif", "ref"),
    #          col=c("orange", "black"),lty = c(1,1), cex=0.8)
    # }
    # if(any(names(Strata) == "k")){
    #   points(X[Strata$k,1],rep(0,length(Strata$k)),pch = 16,col = "blue")
    #   legend("topleft", legend=c("modif", "ref","k"),
    #          col=c("orange", "black","blue"),lty = c(1,1,1), cex=0.8)
    #
    # }

  }else{


    X_dat <- as.data.frame(X)
    colnames(X_dat) <- c("x","y")

    require(ggalt)
    # p <- ggplot()
    colors <- c("ref" = "black", "modif" = "orange", "k" = "blue")

    p <- p +
      geom_point(data = X_dat,aes(x = x,y = y),shape = 1,alpha = 0.4)+
      geom_encircle(data=X_dat[Strata$strata,],aes(x=x,y=y), colour = "black", spread=0.04) +
      # geom_point(data = X_dat[l_strata[[i]]$strata,],aes(x = x,y = y),fill = i,colour = i,alpha = 0.4,pch = 16)+
      # geom_polygon(data = X_chull,aes(x = x,y = y),fill = i,colour = "black",alpha = 0.4)+
      geom_point(data = X_dat[Strata$ref,],aes(x = x,y = y, color = "ref"),shape = 16)+
      geom_point(data = X_dat[Strata$modif,],aes(x = x,y = y,color = "modif"),shape = 16)+
      geom_point(data = X_dat[Strata$k,],aes(x = x,y = y,color = "k"),shape = 16)+
      ggtitle("Strata")+
      scale_color_manual(values = colors)+
      theme_systematic()+
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "bottom")
    p


    # plot(X)
    # points(X[Strata$strata,1],X[Strata$strata,2],pch = 16,col = "green")
    # points(X[Strata$modif,1],X[Strata$modif,2],pch = 16,col = "orange")
    # if(any(names(Strata) == "ref")){
    #   points(X[Strata$ref,1],X[Strata$ref,2],pch = 16,col = "black")
    #   legend("topleft", legend=c("modif", "ref"),
    #          col=c("orange", "black"),pch = c(16,16), cex=0.8)
    # }
    # if(any(names(Strata) == "k")){
    #   points(X[Strata$k,1],X[Strata$k,2],pch = 16,col = "blue")
    #   legend("topleft", legend=c("modif","k"),
    #          col=c("orange","blue"), pch = c(16,16), cex=0.8)
    # }
    # if(any(names(Strata) == "k") & any(names(Strata) == "ref")){
    #
    #   points(X[Strata$k,1],X[Strata$k,2],pch = 16,col = "blue")
    #   points(X[Strata$ref,1],X[Strata$ref,2],pch = 16,col = "black")
    #
    #   legend("topleft", legend=c("modif","k","ref"),
    #          col=c("orange","blue","black"), pch = c(16,16,16), cex=0.8)
    # }
  }
  return(p)

}



#' Title
#'
#' @param X
#' @param strata
#'
#' @return
#' @export
#'
#' @examples
plot.l_strata <- function(l_strata,X){
  X_dat <- as.data.frame(X)
  colnames(X_dat) <- c("x","y")
  require(ggplot2)
  require(ggalt)
  p2 <- ggplot()
  p2 <- p2 +
    geom_point(data = X_dat,aes(x = x,y = y),shape = 1,alpha = 0.4)

  for(i in 1:length(l_strata)){
    X_chull <- as.data.frame(X_dat[l_strata[[i]]$strata,])
    X_chull <- X_chull[chull(X_chull),]
    p2 <- p2 +
      geom_encircle(data=X_dat[l_strata[[i]]$strata,],aes(x=x,y=y), colour = i, spread=0.04)+
      # geom_point(data = X_dat[l_strata[[i]]$strata,],aes(x = x,y = y),fill = i,colour = i,alpha = 0.4,pch = 16)+
      # geom_polygon(data = X_chull,aes(x = x,y = y),fill = i,colour = "black",alpha = 0.4)+
      geom_point(data = X_dat[l_strata[[i]]$ref,],aes(x = x,y = y),shape = 16,colour = "black")+
      geom_point(data = X_dat[l_strata[[i]]$modif,],aes(x = x,y = y),shape = 16,colour = "red")
  }

  p2 <- p2 +
    # geom_point(data = X_dat[ref,],aes(x = x,y = y),shape = 16,colour = "black")+
    ggtitle("Raised strata of j")+
    theme_systematic()+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = "none")
  p2


}
