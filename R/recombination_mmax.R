#' Min Max Arithmetical recombination for DE
#' 
#' Implements the "/mmax" (mim-max-arithmetical) recombination for the ExpDE framework
#'
#' @section Warning:
#' This recombination operator evaluates intermediate candidate solutions, 
#' which adds an extra \code{4*popsize} evaluations per iteration.
#' 
#' @section References:
#' F. Herrera, M. Lozano, A. M. Sanchez, "A taxonomy for the crossover
#' operator for real-coded genetic algorithms: an experimental study", 
#' International Journal of Intelligent Systems 18(3) 309-338, 2003.\cr
#' F Herrera, M. Lozano,  J. L Verdegay. 
#' Tuning fuzzy logic controllers by genetic algorithms. 
#' International Journal of Approximate Reasoning, 12(3):299–315, 1995
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param ... optional parameters (unused)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_mmax <- function(X, M, ...) {
  # ========== Error catching and default value definitions
  
  # Get access to variables in the calling environment
  env <- parent.frame()
  
  if (!identical(dim(X), dim(M))) {
    stop("recombination_mmax() requires dim(X) == dim(M)")
  }
  if (all(c("J", "probpars", "nfe") %in% names(env))){
    stop("recombination_mmax() requires calling environment to contain 
         variables J, nfe and probpars")
  }
  
  # ==========
  
  # Define lambda factors
  lambda <- randM(X)
  
  # Generate trial offspring
  H1 <- lambda * X + (1 - lambda) * M
  H2 <- (1 - lambda) * X + lambda * M
  H3 <- pmin(X, M)
  H4 <- pmax(X, M)
  
  
  # Evaluate trial offspring
  f1 <- evaluate_population(probpars = env$probpars, 
                            Pop      = H1)
  
  f2 <- evaluate_population(probpars = env$probpars, 
                            Pop      = H2)
  
  f3 <- evaluate_population(probpars = env$probpars, 
                            Pop      = H3)
  
  f4 <- evaluate_population(probpars = env$probpars, 
                            Pop      = H4)
  
  env$nfe <- env$nfe + 4 * nrow(X)
  
  # Perform recombination
  fbest <- pmin(f1, f2, f3, f4)
  
  # Perform recombination
  Pop.trialx <- X
  Pop.trialx[f1==fbest, ] <- H1[f1==fbest, ]
  Pop.trialx[f2==fbest, ] <- H2[f2==fbest, ]
  Pop.trialx[f3==fbest, ] <- H3[f3==fbest, ]
  Pop.trialx[f4==fbest, ] <- H4[f4==fbest, ]
  
  # Return recombined population
  return (Pop.trialx)
  }
