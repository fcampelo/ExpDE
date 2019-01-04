#' /current-to-pbest mutation for DE
#' 
#' Implements the "/current-to-pbest" mutation for the ExpDE framework
#' 
#' This routine also implements one special case: 
#' \itemize{
#'  \item current-to-best mutation (\code{current_to_best}), by setting 
#'    \code{mutpars$p = 1}); 
#'  \item Flat recombination (\code{flat}), by setting 
#'    \code{recpars$alpha = recpars$beta = 0})
#' }
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
#' of population \code{X} (\code{J}) in the list \code{L}. This 
#' variable must be defined for \code{mutation_best()} to work. 
#' 
#' @param X population matrix
#' @param J performance vector for population \code{X}
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

mutation_current_to_pbest <- function(X, J, mutpars){

  # ========== Error catching and default value definitions
  
  assertthat::assert_that(is.numeric(mutpars$p), 
                          is_within(mutpars$p, 0, nrow(X), strict = TRUE))

  if (is_within(mutpars$p, 0, 1, strict = TRUE)){
    mutpars$p <- ceiling(mutpars$p * nrow(X))
  }
  # ==========
  
  # Indices to the p-best vectors
  ibest <- order(J)[1:mutpars$p]
  
  # Matrix indices for mutation (x_{pbest}, x_{r1}, x_{r2})
  R <- mapply(FUN     = function(x, i, ibest) {c(sample.int(ibest, 1), i, 
                                                 sample.int(x, 2, replace = FALSE))},
              x       = rep(nrow(X), 
                            times = nrow(X)),
              i       = 1:nrow(X),
              MoreArgs = list(ibest),
              SIMPLIFY = FALSE)

    
  # Auxiliary function: make a single mutation
  pbestmut <- function(pos, Pop, f){
    diffs <- matrix(pos,
                    ncol  = 2,
                    byrow = TRUE)
    return(Pop[pos[2], ] + 
             colSums(f * (Pop[diffs[, 1], ] - Pop[diffs[, 2], ])))
  }

  # Apply mutation
  M <- lapply(R, 
              FUN    = pbestmut, 
              Pop    = X, 
              f      = mutpars$f)
  
  return(matrix(data  = unlist(M), 
                nrow  = nrow(X), 
                byrow = TRUE))
}
