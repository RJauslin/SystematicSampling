#' Find all strata that cover in any way the re unit
#'
#'
#'
#' @param X sptaial coordinates
#' @param tb a shift
#' @param ref the referenced unit
#' @param bound the bound for the sum of inclusion probabilites
#' @param tore a logcial that see the spatial coordinates on tore
#' @param toreBound the bound of the tore
#' @param pik inclusion probabilies
#' @param pikInit
#' @param multi
#' @param comment
#'
#' @return
#' @export
#'
#' @examples
allStrata <- function(X,pik,ref,tb,pikInit,
                      bound = 1.0,multi = TRUE,tore = FALSE,toreBound = -1.0,
                      comment = FALSE){



  ##----------------------------------------------------------------
  ##                          Initializing                         -
  ##----------------------------------------------------------------


  N <- nrow(X)
  eps <- 1e-9
  l_Strata <- list()
  ll <- 1

  #################### ALL COMBINAISION
  if(multi == TRUE){
    tbl <- combTb(tb)
  }else{
    tbl = matrix(tb,nrow = 1)
  }


  ##---------------------------------------------------------------
  ##                          Main loop                           -
  ##---------------------------------------------------------------


  for(tt in 1:nrow(tbl)){
    # print(as.vector(as.numeric(tbl[tt,])))
    # d_ref <- distRef(X,ref,as.vector(as.numeric(tbl[tt,])),tore,toreBound)
    for(k in 1:nrow(X)){
      # print(k)
      if(pik[k]> eps){# we can't loop on null unit and on the
      # d <- distRef(X,k,as.vector(as.numeric(tbl[tt,])),tore,toreBound)
      # tmp <- strataUnitk(d,d_ref,pik,ref,k,pikInit,bound)
        tmp <- strataCompleteLink(X = X,pik = pik,ref = ref,k = k,tb = as.vector(as.numeric(tbl[tt,])),
                              pikInit,bound = bound,tore = tore,toreBound = toreBound)

        if(!is.null(tmp)){
          l_Strata[[ll]] <- tmp
          ll <- ll + 1
        }
      }
    }
  }

  class(l_Strata) <- "l_strata"
  return(l_Strata)
}
