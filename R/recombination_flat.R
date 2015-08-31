#' Flat recombination for DE
#' 
#' Implements the "/flat" (Flat) recombination for the ExpDE 
#' framework
#' 
#' @section Recombination Parameters:
#' Does not require parameters
#'    
#'
#' @section References: 
#' Picek, S.; Jakobovic, D.; Golub, M., "On the recombination operator 
#' in the real-coded genetic algorithms," in Evolutionary Computation (CEC),
#' 2013 IEEE Congress on , vol., no., pp.3103-3110, 20-23 June 2013
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_flat<- function(X, M, ...) {

  # ========== Error catching and default value definitions
  if (!identical(dim(X), dim(M))) {
    stop("recombination_flat() requires dim(X) == dim(M)")
  }
  # ==========
  
  # Determine maximum and minimum values
  Cmin <- pmin(X, M)
  Cmax <- pmax(X, M)
  I    <- Cmax - Cmin
 
  # Return recombined population
  return (Cmin  + randM(X) * I)
}
