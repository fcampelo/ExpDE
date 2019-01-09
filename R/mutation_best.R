#' /best mutation for DE
#' 
#' Implements the "/best/nvecs" mutation for the ExpDE framework
#' 
#' @section Mutation Parameters:
#' The \code{mutpars} parameter contains all parameters required to define the 
#' mutation. \code{mutation_best()} understands the following fields in 
#' \code{mutpars}:
#' \itemize{
#'    \item \code{f} : scaling factor for difference vector(s).\cr
#'    Accepts numeric vectors of size 1 or \code{nvecs}.
#'    \item \code{nvecs} : number of difference vectors to use.\cr 
#'        Accepts \code{1 <= nvecs <= (nrow(X)/2 - 2)}\cr
#'        Defaults to 1.
#' }
#' 
#' @section Warning:
#' This routine will search for the performance vector 
#' of population \code{X} (\code{J}) in the list \code{L}. This 
#' variable must be defined for \code{mutation_best()} to work. 
#' 
#' @Section X:
#' Population matrix (original).
#' @section J:
#' Performance vector for population \code{X}.
#' 
#' @param L list with all parameters for ExpDE framework
#' @param mutpars mutation parameters (see \code{Mutation parameters} for details)
#' 
#' @return Matrix \code{M} containing the mutated population
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#' 
#' @section References:
#' K. Price, R.M. Storn, J.A. Lampinen, "Differential Evolution: A 
#' Practical Approach to Global Optimization", Springer 2005
#' 
#' @export

mutation_best <- function(L, mutpars){
  X <- L$X
  J <- L$J
  
  # ========== Error catching and default value definitions
  assertthat::assert_that(mutpars$nvecs < (nrow(X)/2 - 2))
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
  x.best <- X[J == min(J), ]

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
