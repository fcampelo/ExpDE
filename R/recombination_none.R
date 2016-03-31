#' NULL recombination for DE
#' 
#' Implements the "/none" recombination (i.e., no recombination performed) 
#' for the ExpDE framework
#' 
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param ... optional parameters (unused)
#' 
#' @return The same matrix \code{M} used as an input.
#' 
#' @export

recombination_none <- function(X, M, ...) {
  
  # ========== Error catching and default value definitions
  if (!identical(dim(X), dim(M))) {
    stop("recombination_arith() requires dim(X) == dim(M)")
  }
  # ==========
  
  # Return unmodified mutant population
  return(M)
}
