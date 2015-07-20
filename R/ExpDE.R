#' Differential Evolution Optimization (DE)
#' 
#' Is a method that optimizes a problem by iteratively trying to 
#' improve a candidate solution with regard to a given measure of 
#' quality via the Differential Evolution algorithm.
#' 
#' @param popsize a number containing the size of population for optimization.
#' @param mutpars a list of mutation parameter containing:
#' \code{name} is name of mutation; \code{f} a number with mutation the scaling factor.
#' @param recpars a list of recombination parameter containing:  
#' \code{name} is the type of recombination to be used in the method, ....
#' @param selpars a list of selection parameter containing:
#' \code{name} is the type of selection to be in the method, ....
#' @param convcrit a list of convergence criteria for the method containing: 
#' \code{type} is the type of convergence criterion; 
#' \code{pars} a list with value the type of convergence criterion containing:
#' \code{niter} is value of convergence criteria for the method.
#' \code{stab} is value of stabilization for the method.
#' @param probpars a list problem parameter containing:
#' \code{name} is problem name to be analyzed;  
#' \code{lim_inf} is lower limit of the information to be generated;
#' \code{lim_sup} is upper limit of the information to be generated;
#' \code{opt} other problem parameters.
#' 
#' @return result (value) of the optimization.
#' 
#' @keywords de, optimization
#' 
#' @examples
#' ExpDE(popsize = 40, mutpars = list(name = 'rand', f = 0.2), recpars = list(name = 'bin'),
#' selpars = list(name = 'standard'), convcrit = list(types = c('niter', 'stab'),
#'pars = list(niter = 500, nstab = 5)), probpars = list(name = 'myfun', lim_inf = -5.12, lim_sup = 5.12, opt = 0))
#'
ExpDE <- function(popsize = 40, mutpars = list(name = "rand", f = 0.2), recpars = list(name = "bin"), 
    selpars = list(name = "standard"), convcrit = list(types = c("niter", "stab"), 
        pars = list(niter = 500, nstab = 5)), probpars = list(name = "rastrigin", 
        lim_inf = -5.12, lim_sup = 5.12, opt = 3)) {
    # Differential evolution - a simple and efficient adaptive scheme for global
    # optimization over continuous spaces.  Storn e Price(1995) Rainer Storn e
    # Kenneth Price.  Technical report, International Computer Science Institute
    
    # Federal University of Minas Gerais Department of Electrical Engineering Moises
    # Botelho and Felipe Campelo, Ph.D.
    
    
    # Generation the initial population
    X <- population(popsize, probpars)
    # Evaluate the initial population
    J <- do.call(probpars$name, args = list(X))
    
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
