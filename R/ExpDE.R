#' Experimental Differential Evolution - ExpDE
#' 
#' Modular implementation of the Differential Evolution Algorithm
#' 
#' The detailed description comes here...
#' "Is a method that optimizes a problem by iteratively trying to 
#' improve a candidate solution with regard to a given measure of 
#' quality via the Differential Evolution algorithm."
#' 
#' @param popsize population size
#' @param mutpars list of named mutation parameters. 
#'    See \code{Mutation parameters} for details.
#' @param recpars list of named recombination parameters. 
#'    See \code{Recombination parameters} for details.
#' @param selpars list of named selection parameters. 
#'    See \code{Selection parameters} for details.
#' @param convcrit list of named convergence criteria parameters. See 
#'    \code{Convergence criteria} for details.
#' @param probpars list of named problem parameters.
#'    See \code{Problem description} for details.
#' 
#' @return result (value) of the optimization.
#' 
#' @examples
#' ExpDE(popsize = 40, mutpars = list(name = 'rand', f = 0.2), 
#'    recpars = list(name = 'bin'), selpars = list(name = 'standard'), 
#'    convcrit = list(types = c('niter', 'stab'), 
#'    pars = list(niter = 500, nstab = 5)), probpars = list(name = 'myfun', 
#'    lim_inf = rep(-5.12,2), lim_sup = rep(5.12,2), opt = c(3,3)))
#'
#' @export

ExpDE <- function(popsize, 
                  mutpars  = list(name = "rand", 
                                  f = 0.2), 
                  recpars  = list(name = "bin"), 
                  selpars  = list(name = "standard"), 
                  convcrit, 
                  probpars)
{
    # Differential evolution - a simple and efficient adaptive scheme for global
    # optimization over continuous spaces.  Storn e Price(1995) Rainer Storn e
    # Kenneth Price.  Technical report, International Computer Science Institute
    
    # Federal University of Minas Gerais Department of Electrical Engineering Moises
    # Botelho and Felipe Campelo, Ph.D.
    
    
  # Generate initial population
  X <- create_population(popsize, 
                         probpars)
  
  # Evaluate the initial population
  J <- do.call(probpars$name, 
               args = list(X))
    
    # Generation
    if (convcrit$types[1] == "niter") {
        
      iter <- convcrit$pars$niter
    }
    else
    {
      iter <- convcrit$pars$stab
    }
        for (t in 1:iter) {
            # Mutation  
            M <- do.call(mutpars$name, args = list(X, mutpars))
            # Recombination
            U <- do.call(recpars$name, args = list(X, M))
            # Evaluate U
            G <- evaluate(probpars, U)
            # Selection
            next.pop <- do.call(selpars$name, args = list(U, G, X, J))
            
            X <- next.pop$X
            J <- next.pop$J
            
        }
    
    return(X)
} 
