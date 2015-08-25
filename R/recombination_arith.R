#' Arithmetic recombination for DE
#' 
#' Implements the "/arith" (arithmetic) recombination for the ExpDE framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_arith()} understands the following 
#' fields in recpars:
#'    - \code{cr} : component-wise probability of selection as a cut-point.
#'    Accepts numeric value \code{0 < cr <= 1}.
#'    - \code{K} : arithmetic constant. Defaults to NULL.
#'    Accepts real value \code{0 <= K <= 1}; or \code{K = NULL} for randomly choosing a 
#'   for each individual of population.
#'
#' @section References:
#' F. Herrera, M. Lozano, A. M. Sanchez, "A taxonomy for the crossover
#' operator for real-coded genetic algorithms: an experimental study", 
#' International Journal of Intelligent Systems 18(3) 309-338, 2003.
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population

recombination_arith <- function(X, M, recpars = list(K = NULL)) {

  # ========== Error catching and default value definitions
  if (!("K" %in% names(recpars))) {
    recpars$K <- NULL
  
    if (!is.null(recpars$K)) {
    
      if(!(0 <= recpars$K & recpars$K <= 1)){
      stop("recombination_arith() requires 0 <= recpars$K <= 1")
    }
    
    if(is.character(all.equal(recpars$K, as.numeric(recpars$K)))) {
      stop("recombination_arith() requires an numeric value for K")
    }
    }
  }
  
  if (!("cr" %in% names(recpars))){
    stop("recombination_arith() requires field cr in recpars")
  }
  
  if (!identical(dim(X), dim(M))) {
    stop("recombination_arith() requires dim(X) == dim(M)")
  }
  # ==========
  
  # Perform recombination (depending on the value of recpars$K)
  if(is.null(recpars$K)) {
    # Matrix of recombination multipliers
    recpars$K <-  matrix(rep(runif(n = nrow(X),
                                min = 0, 
                                max = 1),
                        times = ncol(X)),
                    nrow  = nrow(X),
                    byrow = FALSE)
  } 
  
   # Matrix of recombination rate
  rate <- matrix(runif(n = prod(dim(X))) < recpars$cr, 
         nrow = nrow(X))
 
  # Return recombined population
  return (((1 - recpars$K) * X + recpars$K * M)* rate + X * !rate)
}
