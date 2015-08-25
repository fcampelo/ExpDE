#' Blend Alpha recombination for DE
#' 
#' Implements the "/blxAlpha" (Blend Alpha) recombination for the ExpDE framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_blxAlpha()} understands the following 
#' fields in recpars:
#'    - \code{cr} : component-wise probability of selection as a cut-point.
#'    Accepts numeric value \code{0 < cr <= 1}.
#'    - \code{K} : blxAlpha constant. Defaults to 0.5.
#'    Accepts real value \code{0 <= K <= 1}; or \code{K = NULL} for choosing 0.5 default 
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

recombination_blxAlpha <- function(X, M, recpars = list(K = NULL, cr)) {

  # ========== Error catching and default value definitions
  if (!("K" %in% names(recpars))) {
    recpars$K <- NULL
    
    if (!is.null(recpars$K)) {
      
      if(!(0 <= recpars$K & recpars$K <= 1)){
        stop("recombination_blxAlpha() requires 0 <= recpars$K <= 1")
      }
      
      if(is.character(all.equal(recpars$K, as.numeric(recpars$K)))) {
        stop("recombination_blxAlpha() requires an numeric value for K")
      }
    }
  }
  
  if (!("cr" %in% names(recpars))){
    stop("recombination_blxAlpha() requires field cr in recpars")
  }
  
  if (!identical(dim(X), dim(M))) {
    stop("recombination_blxApha() requires dim(X) == dim(M)")
  }
  # ==========
  
  # Perform recombination (depending on the value of recpars$K)
  if(is.null(recpars$K)) {
    # default value alpha
    recpars$K <-  0.5
  } 
  
  # Matrix of recombination rate
  rate <- matrix(runif(n = prod(dim(X))) < recpars$cr, 
                 nrow = nrow(X))
  
  
  setfun <- function(n, x, m, alpha) {
    I <- max(x, m) - min(x, m)
    
    z1 <- (min(x, m) - I * alpha)  
    z2 <- (max(x, m) + I * alpha)
    
    return (runif(n = 1,
                  min = z1,
                  max = z2))
  }
  
  # Recombination matrix - using mapply() to apply over multiple indexed objects
  R <- (mapply(FUN      = setfun, 
                x        = X,
                m        = M,
            alpha        = recpars$K, 
                SIMPLIFY = TRUE))
  
  
  R <- matrix(R, nrow = nrow(X), byrow = T)
 
  # Return recombined population
  return (R* rate + X * !rate)
}
