#' /simBinary recombination for DE
#' 
#' Implements the "/simBinary" (Simulated Binary) recombination for the ExpDE framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_simBinary()} understands the following field in 
#' recpars:
#'    - \code{eta} : component-wise probability.
#'    Accepts numeric value \code{ eta > 0}.
#'    
#'
#' @section References:
#' K. Price, R.M. Storn, J.A. Lampinen, "Differential Evolution: A 
#' Practical Approach to Global Optimization", Springer 2005
#' 
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_simBinary <- function(X, M, recpars) {

  # ========== Error catching and default value definitions
  if (!("eta" %in% names(recpars))){
    stop("recombination_simBinary() requires field eta in recpars")
  }
  if (! (recpars$eta > 0)) {
    stop("recombination_simBinary() requires numeric  recpars$eta > 0")
  }
  if (!identical(dim(X),dim(M))) {
    stop("recombination_simBinary() requires dim(X) == dim(M)")
  }

  # ==========
  p <- randM(X);
  #Define beta parameters
  beta <- rep((p<=0.5) * (2 * p) ^ (1 / (recpars$eta + 1)) + 
               (p > 0.5) * (2 * (1 - p)) ^ (1 / (recpars$eta + 1)), 1 ,ncol(X));
  
  dir <- sign(0.5 - runif(1));
  
  # Return recombined population
  return(0.5 * ((1 + dir * beta) * X + (1 - dir * beta) * M ))
}
