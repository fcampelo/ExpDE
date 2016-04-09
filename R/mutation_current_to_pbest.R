#' /current-to-pbest mutation for DE
#' 
#' Implements the "/current-to-pbest" mutation for the ExpDE framework
#' 
#' @section Mutation Parameters:
#' The \code{mutpars} parameter contains all parameters required to define the 
#' mutation. \code{mutation_current_to_pbest()} understands the following fields in 
#' \code{mutpars}:
#' \itemize{
#'    \item \code{f} : scaling factor for difference vector(s).\cr
#'    Accepts numeric vectors of size 1 or \code{nvecs}.
#'    \item \code{p} : either the number of "best" vectors to use (if given as a 
#'    positive integer) or the proportion of the population to use as "best"
#'    vectors (if 0 < p < 1).
#' }
#' 
#' @section Warning:
#' This routine will search for the performance vector 
#' of population \code{X} (\code{J}) in the parent environment (using 
#' \code{parent.frame()}. This variable must be defined for 
#' \code{mutation_current_to_pbest()} to work. 
#' 
#' @param X population matrix
#' @param mutpars mutation parameters (see \code{Mutation parameters} for details)
#' 
#' @return Matrix \code{M} containing the mutated population
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#' 
#' @section References:
#' J. Zhang, A.C. Sanderson, 
#' "JADE: Adaptive differential evolution with optional external archive". 
#' IEEE Transactions on Evolutionary Computation 13:945-958, 2009
#' 
#' @export

mutation_current_to_pbest <- function(X, mutpars){

  # Get access to variables in the calling environment
  env <- parent.frame()
  
  # ========== Error catching and default value definitions
  if (!("f" %in% names(mutpars))){
    stop("mutation_current_to_pbest() requires field f in mutpars")
  }
  if (length(mutpars$f) == 1) mutpars$f <- rep(mutpars$f, 
                                               mutpars$nvecs)
  
  
  # STOPPED HERE =====
  if (!is.numeric(mutpars$p) || p <= 0){
    stop("mutation_current_to_pbest() requires parameter p to be either integer or a numeric value between 0 and 1")
  }
  if (mutpars$p)
  # ==========
  
  # Matrix indices for mutation (r1 != r2 != r3 != ... != rn)
  R <- lapply(X       = rep(nrow(X), 
                            times = nrow(X)),
              FUN     = sample.int,
              size    = 2 * mutpars$nvecs,
              replace = FALSE)

    
  # Auxiliary function: make a single mutation
  bestmut <- function(pos, Pop, f, x.best){
    diffs <- matrix(pos,
                    ncol  = 2,
                    byrow = TRUE)
    if (nrow(diffs) == 1) {
      wdiffsum <- f * (Pop[diffs[, 1], ] - Pop[diffs[, 2], ])
    } else {
      wdiffsum <- colSums(f * (Pop[diffs[, 1], ] - Pop[diffs[, 2], ]))
    }
    return(x.best + wdiffsum)
  }
  #individual best
  x.best <- X[env$J == min(env$J), ]

  #use only one base vector if there is more than one "best"
  if(is.matrix(x.best)){
    x.best <- x.best[sample.int(nrow(x.best), size = 1), ]
  }

  # Apply mutation
  M <- lapply(R, 
              FUN    = bestmut, 
              Pop    = X, 
              f      = mutpars$f,
              x.best = x.best)
  
  return(matrix(data  = unlist(M), 
                nrow  = nrow(X), 
                byrow = TRUE))
}
