#' Blend Alpha Beta recombination for DE
#' 
#' Implements the "/blxAlphaBeta" (Blend Alpha Beta) recombination for the ExpDE 
#' framework
#' 
#' This routine also implements two special cases: 
#' \itemize{
#'  \item BLX-alpha recombination (\code{blxAlpha}), by setting 
#'    \code{recpars$alpha = recpars$beta}); 
#'  \item Flat recombination (\code{flat}), by setting 
#'    \code{recpars$alpha = recpars$beta = 0})
#' }
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_blxAlpha()} understands the following 
#' fields in \code{recpars}:
#' \itemize{
#'    \item \code{alpha} : extrapolation parameter for 'best' parent vector.\cr
#'    Accepts real value \code{0 <= alpha <= 0.5}.
#'    \item \code{beta} : extrapolation parameter for 'worst' parent vector.\cr
#'    Accepts real value \code{0 <= beta <= 0.5}. 
#' }
#' 
#'  @section Warning:
#'  This recombination operator evaluates the candidate solutions in \code{M}, 
#'  which adds an extra \code{popsize} evaluations per iteration.
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

recombination_blxAlphaBeta <- function(X, M, recpars) {
  
  # Get access to variables in the calling environment
  env <- parent.frame()

  # ========== Error catching and default value definitions
  assertthat::assert_that(is.matrix(X), is.numeric(X),
                          is.matrix(M), is.numeric(M),
                          assertthat::are_equal(dim(X), dim(M)),
                          all(assertthat::has_name(recpars, 
                                                   c("alpha", "beta"))),
                          is.numeric(recpars$alpha), is.numeric(recpars$beta),
                          is_within(recpars$alpha, 0, 0.5),
                          is_within(recpars$beta, 0, 0.5),
                          all(assertthat::has_name(env, 
                                                   c("J", "probpars", "nfe"))))
  # ==========
  
  # Performance values of the current population (X)
  f.X <- env$J
    
  #Evaluate population M
  f.M <- evaluate_population(probpars = env$probpars, 
                             Pop      = M)
  
  # Update NFE counter in calling environment
  env$nfe <- env$nfe + nrow(M)
  
  # Get best parent indicator matrix
  X.is.best <- matrix(rep(f.X <= f.M,
                          times = ncol(X)),
                      ncol  = ncol(X),
                      byrow = FALSE)
  
  # Get infimum and supremum values, and interval lengths
  Cmin <- pmin(X, M)
  Cmax <- pmax(X, M)
  I    <- Cmax - Cmin
  
  # Get 'best' and 'worst' parents
  C1 <- X * X.is.best + M * !X.is.best
  C2 <- M * X.is.best + X * !X.is.best
  
  S <- (C1 <= C2)
 
  # Return recombined population 
  return(pmin(C1, C2) - 
           I * (recpars$alpha * S + recpars$beta * !S) + 
           randM(X) * (I * ( 1 + recpars$alpha + recpars$beta)))
}
