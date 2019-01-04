#' Standard selection for DE
#' 
#' Implements the standard selection (greedy) for the ExpDE framework
#' 
#'        
#' @section X: 
#' Population matrix (original).
#' @section U: 
#' Population matrix (recombined). 
#' @section J: 
#' Performance vector for population \code{X}.
#' @section G: 
#' Performance vector for population \code{U}.
#' 
#' @param L list with all parameters for ExpDE framework
#' 
#' @return List \code{L} containing all updated parameters, including
#' list object containing the selected population (\code{Xsel}) and 
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