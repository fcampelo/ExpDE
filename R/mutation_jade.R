#' /current-to-pbest mutation for adaptive DE
#' 
#' Implements the "/current-to-pbest" mutation with changes to the use
#' of the JADE self-adaptation method
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
#' mutation. \code{mutation_jade()} understands the following fields in 
#' \code{mutpars}:
#' \itemize{
#'    \item \code{p} : either the number of "best" vectors to use (if given as a 
#'    positive integer) or the proportion of the population to use as "best"
#'    vectors (if 0 < p < 1). \cr
#' }
#' \code{adapars}:
#' \itemize{
#'    \item \code{Fi} : set of scaling factor of each individual for difference
#'     vector(s). 
#' }
#' 
#' @section Warning:
#' This routine will search for the performance vector 
#' of population \code{X} (\code{J}) in the list \code{L}. This 
#' variable must be defined for \code{mutation_jade()} to work. 
#' 
#' @section X:
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
#' J. Zhang, A.C. Sanderson, 
#' "JADE: Adaptive differential evolution with optional external archive". 
#' IEEE Transactions on Evolutionary Computation 13:945-958, 2009
#' 
#' @export

mutation_jade <- function(L, mutpars){
  X <- L$X
  J <- L$J
  
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
  pbestmut <- function(pos, Pop, file, f){
    diffs <- matrix(pos,
                    ncol  = 2,
                    byrow = TRUE)
    Pop2  <- sample(x       = c(Pop, file), 
                    size    = length(Pop), 
                    replace = FALSE)
    
    Pop2  <- matrix(data = Pop2,
                    nrow = nrow(Pop),
                    ncol = ncol(Pop))
    
    #To do:
    #How to ensure that Pop[diffs[, 1], ] - Pop2[diffs[, 2], ] are different
    
    return(Pop[pos[2], ] +
             colSums(f[pos[2],] * (Pop[diffs[, 1], ] - Pop2[diffs[, 2], ]))) 
  }
  
  # Apply mutation
  M <- lapply(R, 
              FUN    = pbestmut, 
              Pop    = X,
              file   = L$files$A,
              f      = L$adapars$Fi)
  
  return(matrix(data  = unlist(M), 
                nrow  = nrow(X), 
                byrow = TRUE))
}
