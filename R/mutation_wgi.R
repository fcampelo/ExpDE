#' /wgi mutation for DE
#' 
#' Implements the "/wgi/nvecs" mutation (weighted global intermediate) for the 
#' ExpDE framework. This variant is based on a recombination strategy known as
#' "weighted global intermediate recombination" (see the References section for 
#' details)
#' 
#' @section Mutation Parameters:
#' The \code{mutpars} parameter contains all parameters required to define the 
#' mutation. \code{mutation_wgi()} understands the following fields in 
#' \code{mutpars}:
#' \itemize{
#'    \item \code{f} : scaling factor for difference vector(s).\cr
#'    Accepts numeric vectors of size 1 or \code{nvecs}.
#'    \item \code{nvecs} : number of difference vectors to use.\cr 
#'        Accepts \code{1 <= nvecs <= (nrow(X)/2 - 2)}\cr
#'        Defaults to 1.
#' }
#' 
#' @param X population matrix
#' @param mutpars mutation parameters (see \code{Mutation parameters} for 
#' details)
#' 
#' @return Matrix \code{M} containing the mutated population
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#' 
#' @section References:
#' D. Arnold,
#' "Weighted multirecombination evolution strategies". 
#' Theoretical Computer Science 361(1): 18-37, 2006.
#' 
#' T. Glasmachers, C. Igel,
#' "Uncertainty handling in model selection for support vector machines". 
#' Proc. International Conference on Parallel Problem Solving from Nature 
#' (PPSN'08), 185-194, 2008.
#' 
#' @export

mutation_wgi <- function(X, mutpars){

  # ========== Error catching and default value definitions
  
  # Get access to variables in the calling environment
  env <- parent.frame()
  
  if (!("nvecs" %in% names(mutpars))) mutpars$nvecs <- 1
  
  assertthat::assert_that(is.matrix(X), is.numeric(X),
                          assertthat::is.count(mutpars$nvecs),
                          mutpars$nvecs < (nrow(X)/2 - 2),
                          assertthat::has_name(mutpars, "f"),
                          is.numeric(mutpars$f))

  if (length(mutpars$f) == 1) mutpars$f <- rep(mutpars$f, 
                                               mutpars$nvecs)
  # ==========
  
  # Set weights
  w <- log(nrow(X) + 1) - log(1:nrow(X))
  W <- matrix(rep(w / sum(w), times = ncol(X)),
              nrow  = nrow(X),
              byrow = FALSE)
  
  # Define basis vector (weighted global intermediate)
  x.basis  <- colSums(X[order(env$J), ] * W)
  
  # Matrix indices for mutation (r1 != r2 != r3 != ... != rn)
  R <- lapply(X = rep(nrow(X), 
                      times = nrow(X)),
              FUN     = sample.int,
              size    = 2 * mutpars$nvecs,
              replace = FALSE)

    
  # Auxiliary function: make a single mutation
  wgimut <- function(pos, Pop, x.basis, f){
    diffs <- matrix(pos,
                    ncol  = 2,
                    byrow = TRUE)
    if (nrow(diffs) == 1) {
      wdiffsum <- f * (Pop[diffs[, 1], ] - Pop[diffs[, 2], ])
    } else {
      wdiffsum <- colSums(f * (Pop[diffs[, 1], ] - Pop[diffs[, 2], ]))
    }
    return(x.basis + wdiffsum)
  }
  
  # Apply mutation
  M <- lapply(R, 
              FUN     = wgimut, 
              Pop     = X, 
              x.basis = x.basis,
              f       = mutpars$f)
  
  return(matrix(data  = unlist(M), 
                nrow  = nrow(X), 
                byrow = TRUE))
}
