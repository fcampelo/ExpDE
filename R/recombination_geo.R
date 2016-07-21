#' Geometric recombination for DE
#' 
#' Implements the "/geo" (geometric) recombination for the ExpDE framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_geo()} understands the following 
#' fields in \code{recpars}:
#' \itemize{
#'    \item \code{rho} : exponent for geometrical recombination.\cr
#'    Accepts numeric value \code{0 <= rho <= 1} or \code{NULL} (in which 
#'    case a random value is chosen for each recombination).
#'}
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

recombination_geo <- function(X, M, recpars) {
  
  # ========== Error catching and default value definitions
  assertthat::assert_that(is.matrix(X), is.numeric(X),
                          is.matrix(M), is.numeric(M),
                          assertthat::are_equal(dim(X), dim(M)),
                          assertthat::has_name(recpars, "rho"),
                          is.null(recpars$rho) || is_within(recpars$rho, 0, 1))
  # ==========
  
  # Get all values to the interval [.25, .75] before performing the recombination
  # (the resulting recombined matrix must be later restored to the original 
  # range)
  mins <- pmin(X, M)
  maxs <- pmax(X, M)
  eps  <- 1e-15
  X    <- 0.25 + 0.5 * (X - mins) / (maxs - mins + eps)
  M    <- 0.25 + 0.5 * (M - mins) / (maxs - mins + eps)
  
  if(is.null(recpars$rho)){ # use a random value for each recombination
    rho <- matrix(rep(stats::runif(nrow(X)),
                      times = ncol(X)),
                  ncol = ncol(X))
  } else{ # use the given (or default) rho value for all recombinations
    rho <- recpars$rho + 0 * X
  }
  
  
  # Randomize which parent will use exponent rho and which will use 
  # (1-rho)
  inv.rho        <- as.logical(round(stats::runif(nrow(X))))
  rho[inv.rho, ] <- 1 - rho[inv.rho, ]
  
  # Build recombined population and return it to the original range
  U <- X ^ rho * M ^ (1 - rho)
  return(mins + (U - 0.25) * (maxs - mins) / 0.5)
}
