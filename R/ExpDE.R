#' Experimental Differential Evolution - ExpDE
#'
#' Modular implementation of the Differential Evolution Algorithm
#'
#' The detailed description comes here...
#'
#' @section Mutation Parameters:
#' Here comes a description of the \code{mutpars} structure, with a link to the
#' vignette (or at least to a list of available operators)
#'
#' @section Recombination parameters:
#' Here comes a description of the \code{recpars} structure, with a link to the
#' vignette (or at least to a list of available operators)
#'
#' @section Selection parameters:
#' Here comes a description of the \code{selpars} structure, with a link to the
#' vignette (or at least to a list of available operators)
#'
#' @section Stop criteria:
#' Here comes a description of the \code{stopcrit} structure, with a link to the
#' vignette (or at least to a list of available criteria)
#'
#' @section Problem description:
#' Here comes a description of the \code{probpars} structure.
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
#'    See \code{Problem Description} for details.
#'
#' @return A list object containing the final population (sorted by performance)
#', the performance vector, and some run statistics.
#' @author Felipe Campelo and Moises Botelho
#'
#' @examples
#' # DE/rand/1/bin with population 40, F = 0.8 and CR = 0.5
#' popsize  <- 40
#' mutpars  <- list(name = "mutation_rand", f = 0.8)
#' recpars  <- list(name = "recombination_bin", cr = 0.5, minchange = TRUE)
#' selpars  <- list(name = "selection_standard")
#' stopcrit <- list(names = "stop_maxiter", maxiter = 100)
#' probpars <- list(name   = "sphere",
#'                 xmin = rep(-5.12,4), xmax = rep(5.12,4))
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars)
#'
#' # DE/rand/1/exp
#' recpars  <- list(name = "recombination_exp", cr = 0.2)
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars)
#'
#' # DE/rand/2/blxAlpha
#' recpars  <- list(name = "recombination_blxAlpha", alpha = 0.1)
#' mutpars  <- list(name = "mutation_rand", f = 0.8, nvecs = 2)
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars)
#' 
#' # DE/rand/2/blxAlphaBeta
#' recpars <- list(name = "recombination_blxAlphaBeta", alpha = 0.05, beta = 0.2)
#' stopcrit <- list(names = "stop_maxeval", maxevals = 100*popsize)
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars) 
#' 
#' # DE/rand/1/sbx
#' recpars  <- list(name = "recombination_sbx", eta = 10)
#' mutpars$nvecs <- 1
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars)
#' 
#' # DE/rand/1/flat
#' recpars  <- list(name = "recombination_flat")
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars)
#'
#' # DE/rand/1/hx
#' recpars  <- list(name = "recombination_hx")
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars)
#' 
#' # DE/rand/1/lbgax
#' recpars  <- list(name = "recombination_lbgax")
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars)
#' 
#' # DE/rand/1/linear
#' recpars  <- list(name = "recombination_linear")
#' ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars)
#' @export

ExpDE <- function(popsize,
                  mutpars  = list(name = "mutation_rand",
                                  f    = 0.2),
                  recpars  = list(name  = "recombination_bin",
                                  cr    = 0.8, 
                                  nvecs = 1),
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
    X <- next.pop$Xsel
    J <- next.pop$Jsel
  }

  X <- denormalize_population(probpars, X[order(J), ])
  J <- sort(J)
  return(list(X     = X,
              Fx    = J,
              Xbest = X[1,],
              Fbest = J[1],
              nfe   = nfe,
              iter  = t))
}
