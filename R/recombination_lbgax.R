#' Linear BGA recombination for DE
#' 
#' Implements the "/lbgax" (Linear Breeder Genetic Algorithm) recombination for the ExpDE 
#' framework
#'
#' @section References:
#' F. Herrera, M. Lozano, A. M. Sanchez, "A taxonomy for the crossover
#' operator for real-coded genetic algorithms: an experimental study", 
#' International Journal of Intelligent Systems 18(3) 309-338, 2003.\cr
#' D. Schlierkamp-voosen , H. Muhlenbein, "Strategy Adaptation by 
#' Competing Subpopulations", Proc. Parallel Problem Solving from Nature 
#' (PPSN III), 199-208, 1994.
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param ... optional parameters (unused)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_lbgax <- function(X, M, ...) {
  # ========== Error catching and default value definitions
  
  # Get access to variables in the calling environment
  env <- parent.frame()
  
  if (!identical(dim(X), dim(M))) {
    stop("recombination_lbgax() requires dim(X) == dim(M)")
  }
  if (all(c("J", "probpars", "nfe") %in% names(env))){
    stop("recombination_lbgax() requires calling environment to contain 
         variables J, nfe and probpars")
  }
  
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
                      ncol = ncol(X),
                      byrow = FALSE)
  
  
  # Get 'best' and 'worst' parents
  C1 <- X * X.is.best + M * !X.is.best
  C2 <- M * X.is.best + X * !X.is.best
  
  # Set recombination parameters
  Lambda <- (C2 - C1) / rep(sqrt(sum((C1 - C2) ^ 2)), ncol(X))
  
  
  R  <- matrix(runif(nrow(X) * 16), ncol = 16)
  mr <- R < (1/16)
  ms <- matrix(data = rep((2^-(0:15)), times = nrow(X)), ncol = 16, byrow = T)
  vs <- apply(X = mr * ms, MARGIN = 1, FUN = sum)  
  
  delta = matrix(data = rep(x = vs, times = ncol(X)), ncol = ncol(X))
              
  dir = sign(0.1-randM(X));
  
  # Return recombined population
  return (C1 + dir * 0.5 * delta * Lambda)
  }
