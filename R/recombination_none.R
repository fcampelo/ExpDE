#' NULL recombination for DE
#' 
#' Implements the "/none" recombination (i.e., no recombination performed) 
#' for the ExpDE framework
#' 
#' @param L list with all parameters for ExpDE framework 
#' @param ... optional parameters (unused)
#' 
#' @return The same matrix \code{M} used as an input.
#' 
#' @export

recombination_none <- function(L, ...) {
  M       = L$M
  # Return unmodified mutant population
  return(M)
}
