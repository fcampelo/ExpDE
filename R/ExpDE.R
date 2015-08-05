#' Experimental Differential Evolution - ExpDE
#' 
#' Modular implementation of the Differential Evolution Algorithm
#' 
#' The detailed description comes here...
#' "Is a method that optimizes a problem by iteratively trying to 
#' improve a candidate solution with regard to a given measure of 
#' quality via the Differential Evolution algorithm."
#' 
#' @section Mutation parameters:
#' 
#' @section Recombination parameters:
#' 
#' @section Selection parameters:
#' 
#' @section Stop criteria:
#' 
#' 
#' @param popsize population size
#' @param mutpars list of named mutation parameters. 
#'    See \code{Mutation parameters} for details.
#' @param recpars list of named recombination parameters. 
#'    See \code{Recombination parameters} for details.
#' @param selpars list of named selection parameters. 
#'    See \code{Selection parameters} for details.
#' @param stopcrit list of named stop criteria parameters. See 
#'    \code{Stop criteria} for details.
#' @param probpars list of named problem parameters.
#'    See \code{Problem description} for details.
#' 
#' @return A list object containing the final population (sorted by performance)
#', the performance vector, and some run statistics.
#' @author Felipe Campelo and Moises Botelho
#' 
#' @examples
#' popsize  <- 40
#' mutpars  <- list(name = "mutation_rand", f = 0.8)
#' recpars  <- list(name = "recombination_bin", cr = 0.5, minchange = TRUE)
#' selpars  <- list(name = "selection_standard")
#' stopcrit <- list(names = "stop_maxiter", maxiter = 100)
#' probpars <- list(name   = "rastrigin", 
#'                 lim_inf = rep(-5.12,2), lim_sup = rep(5.12,2))
#' 
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars)
#' @export

ExpDE <- function(popsize, 
                  mutpars  = list(name = "rand", 
                                  f = 0.2), 
                  recpars  = list(name = "bin"), 
                  selpars  = list(name = "standard"), 
                  stopcrit, 
                  probpars)
{
  
  # Generate initial population
  X <- create_population(popsize  = popsize, 
                         probpars = probpars)
  
  # Evaluate the initial population
  J <- evaluate_population(probpars = probpars, 
                           Pop      = X)
  
  # Prepare for iterative cycle:
  keep.running  <- TRUE     # stop criteria flag
  t             <- 0        # counter: iterations
  nfe           <- popsize  # counter: number of function evaluations
  
  
  # Iterative cycle
  while(keep.running){
    t <- t + 1          # Update iteration counter
    
    # Mutation  
    M <- do.call(mutpars$name, 
                 args = list(X       = X, 
                             mutpars = mutpars))
    
    
    # Recombination
    U <- do.call(recpars$name, 
                 args = list(X       = X, 
                             M       = M,
                             recpars = recpars))
    
    # Evaluate U
    G <- evaluate_population(probpars = probpars, 
                             Pop      = U)
    nfe <- nfe + popsize
    
    # Selection
    next.pop <- do.call(selpars$name, 
                        args = list(X = X, 
                                    U = U, 
                                    J = J, 
                                    G = G))
    
    # Stop criteria
    keep.running <- check_stop_criteria()
    
    # Compose next population
    X <- next.pop$X
    J <- next.pop$J
    
  }
  
  X <- X[order(J), ]
  J <- sort(J)
  return(list(X    = denormalize_population(probpars, X),
              Fx   = sort(J),
              nfe  = nfe,
              iter = t))
} 
