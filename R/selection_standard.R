#' Standard selection for DE
#' 
#' Implements the standard selection (greedy) for the ExpDE framework
#' 
#'        
#' @param X population matrix (original)
#' @param U population matrix (recombined) 
#' @param J performance vector for population \code{X}
#' @param G performance vector for population \code{U}
#' 
#' @return list object containing the selected population (\code{Xsel}) and 
#' its corresponding performance values (\code{Jsel}).
#' 
#' @export

selection_standard <- function(L){
  X <- L$X
  U <- L$U
  J <- L$J
  G <- L$G
  
  sel.vec       <- (G <= J)
  X[sel.vec, ]  <- U[sel.vec, ]
  J[sel.vec]    <- G[sel.vec]
  
  L$nextpop <- list(Xsel = X, 
                    Jsel = J) 
  
  return(L)
}