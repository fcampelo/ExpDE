#' One-point recombination for DE
#' 
#' Implements the one-point recombination (as used in the Simple GA).
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_onepoint()} understands the following 
#' fields in \code{recpars}:
#' \itemize{
#'    \item \code{K} : cut point for crossover.\cr
#'    Accepts integer value \code{0 <= K < n}, where \code{n} is the 
#'    dimension of the problem; Use \code{K = 0} or \code{K = NULL} for randomly 
#'    choosing a position for each pair of points.\cr
#'    Defaults to \code{NULL}.
#'}
#'
#' @section References:
#' F. Herrera, M. Lozano, A. M. Sanchez, "A taxonomy for the crossover
#' operator for real-coded genetic algorithms: an experimental study", 
#' International Journal of Intelligent Systems 18(3) 309-338, 2003.
#'
#' @section X:
#' Population matrix (original).
#' @section M: 
#' Population matrix (mutated).
#' 
#' @param L list with all parameters for ExpDE framework 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_onepoint <- function(L, recpars = list(K = NULL)) {
  X       = L$X
  M       = L$M
  
  # ========== Error catching and default value definitions

  assertthat::assert_that(assertthat::has_name(recpars, "K"),
                          is.null(recpars$K) || 
                            (assertthat::is.count(recpars$K) && is_within(recpars$K, 0, ncol(X) - 1)))
  # ========== 
  
  # Perform recombination (depending on the value of recpars$K)
  if(is.null(recpars$K) || recpars$K == 0) {
    # Matrix of cut points
    cuts <-  matrix(rep(sample.int(n       = ncol(X)-1, 
                                   size    = nrow(X), 
                                   replace = TRUE),
                        times = ncol(X)),
                    nrow  = nrow(X),
                    byrow = FALSE)
    
    # Recombination matrix
    R <- cuts < matrix(rep(1:ncol(X),
                           times = nrow(X),
                           byrow = FALSE),
                       nrow  = nrow(X),
                       byrow = TRUE)
  } else {
    # Recombination matrix
    chgvec <- logical(ncol(X))
    chgvec[1:recpars$K] <- TRUE
    R <- matrix(rep(chgvec, times = nrow(X)),
                nrow  = nrow(X),
                byrow = TRUE)
  }
  
  # Randomize which population will donate the variables with the lowermost 
  # indexes
  if (stats::runif(1) < 0.5){ 
     R <- !R
	 }
        
  # Return recombined population
  return(R * M + (!R) * X) 

}
