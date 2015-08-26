#' Geometric recombination for DE
#' 
#' Implements the "/geo" (geometric) recombination for the ExpDE framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_geo()} understands the following 
#' fields in recpars:
#'    - \code{alpha} : exponent for geometrical recombination. 
#'    Accepts numeric value \code{0 <= alpha <= 1} or \code{NULL} (in which 
#'    case a random value is chosen for each recombination). Defaults to 
#'    \code{alpha = 0.5}.
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
#' 
#' @export

recombination_geo <- function(X, M, recpars = list(alpha = 0.5)) {

  # ========== Error catching and default value definitions
  if (!identical(dim(X), dim(M))) {
    stop("recombination_geo() requires dim(X) == dim(M)")
  }
  if (!("alpha" %in% names(recpars))){
    recpars$alpha <- 0.5
  }
  if (!is.null(recpars$alpha) && !(0 < recpars$alpha && recpars$alpha <= 1)) {
    stop("recombination_geo() requires numeric 0 < recpars$alpha <= 1")
  }
  # ==========
  
  # Repair out-of-bounds vectors
  X <- pmax(0 * X, pmin(0 * X + 1, X))
  X <- pmax(0 * M, pmin(0 * M + 1, M))
  
  if(is.null(recpars$alpha)){ # use a random value for each recombination
    alpha <- matrix(rep(runif(nrow(X)),
                        times = ncol(X)),
                    ncol = ncol(X),
                    byrow = FALSE)
  } else{ # use the given (or default) alpha value for all recombinations
    alpha <- matrix(rep(recpars$alpha,
                        prod(dim(X))),
                    ncol = ncol(X),
                    byrow = FALSE)
  }
  
  
  # Randomize which parent will use exponent alpha and which will use 
  # (1-alpha)
  inv.alpha <- as.logical(round(runif(nrow(X))))
  alpha[inv.alpha, ] <- 1 - alpha[inv.alpha, ]
  
  # Return recombined population
  return(X^alpha * M^(1 - alpha))
}
