
#' Title
#'
#' @param l_Strata 
#'
#' @return
#' @export
#'
#' @examples
print.l_strata <- function(l_strata){
  if(length(l_strata) == 0){
    print(list())
  }else{
    eps <- 1e-13
    n <- length(l_strata)
    dat <- data.frame(ref = rep(0,n),
                      k = rep(0,n),
                      modif = rep(0,n),
                      d_ref_modif = rep(0,n),
                      pik_modif = rep(0,n))
    
    for(i in 1:length(l_strata)){
      dat[i,1] <- l_strata[[i]]$ref
      dat[i,2] <- l_strata[[i]]$k
      dat[i,3] <- l_strata[[i]]$modif
      dat[i,4] <- l_strata[[i]]$d_ref_modif
      dat[i,5] <- l_strata[[i]]$pik_modif
    }
    
    cat("--- Summary of data for strata ---","\n\n")
    print(dat)
    cat("\n\n","--- strata ---","\n\n")
    empty <- lapply(l_strata, FUN = function(x){cat(x$strata, sep = '\t'); cat("\n")})
    cat("\n\n","--- strata_wo_modif ---","\n\n")
    empty <- lapply(l_strata, FUN = function(x){cat(x$strata_wo_modif, sep = '\t'); cat("\n")})
  }
}