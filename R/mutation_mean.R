#' /mean mutation for DE
#' 
#' Implements the "/mean/nvecs" mutation for the ExpDE framework
#' 
#' @section Mutation Parameters:
#' The \code{mutpars} parameter contains all parameters required to define the 
#' mutation. \code{mutation_mean()} understands the following fields in 
#' \code{mutpars}:
#' \itemize{
#'    \item \code{phi} : scaling factor for difference vector(s).\cr
#'    Accepts numeric vectors of size 1 or \code{nvecs}.
#'    \item \code{nvecs} : number of difference vectors to use.\cr 
#'        Accepts \code{1 <= nvecs <= (nrow(X)/2 - 2)}\cr
#'        Defaults to 1.
#' }
#' 
#' @param X population matrix
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

mutation_mean <- function(X, mutpars){
  
  # ========== Error catching and default value definitions
  
  # Get access to variables in the calling environment
  env <- parent.frame()
  
  if (!("nvecs" %in% names(mutpars))) mutpars$nvecs <- 1
  
  assertthat::assert_that(is.matrix(X), is.numeric(X),
                          assertthat::is.count(mutpars$nvecs),
                          mutpars$nvecs < (nrow(X)/2 - 2),
                          assertthat::has_name(mutpars, "phi"),
                          is.numeric(mutpars$phi))
  
  if (length(mutpars$phi) == 1) mutpars$phi <- rep(mutpars$phi, 
                                                   mutpars$nvecs)
  # ==========
  
  # Define basis vector (mean)
  x.basis  <- colMeans(X)
  
  # Matrix indices for mutation (r1 != r2 != r3 != ... != rn)
  R <- lapply(X = rep(nrow(X), 
                      times = nrow(X)),
              FUN     = sample.int,
              size    = 2 * mutpars$nvecs,
              replace = FALSE)
  
  
  # Auxiliary function: make a single mutation
  wgimut <- function(pos, Pop, x.basis, phi){
    diffs <- matrix(pos,
                    ncol  = 2,
                    byrow = TRUE)
    if (nrow(diffs) == 1) {
      wdiffsum <- phi * (Pop[diffs[, 1], ] - Pop[diffs[, 2], ])
    } else {
      wdiffsum <- colSums(phi * (Pop[diffs[, 1], ] - Pop[diffs[, 2], ]))
    }
    return(x.basis + wdiffsum)
  }
  
  # Apply mutation
  M <- lapply(R, 
              FUN     = wgimut, 
              Pop     = X, 
              x.basis = x.basis,
              phi     = mutpars$phi)
  
  return(matrix(data  = unlist(M), 
                nrow  = nrow(X), 
                byrow = TRUE))
}