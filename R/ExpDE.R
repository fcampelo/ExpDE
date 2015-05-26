#' Differential Evolution Algorithm (DE)
#' 
#' Is a method that optimizes a problem by iteratively trying to 
#' improve a candidate solution with regard to a given measure of quality
#' @param popsize size of population.
#' @param mutpars mutation the parameters list containing:
#'  \it{name} is name of mutation; \it{f} mutation the scaling factor.
#' @param recpars type of recombination to be used in the algorithm.
#' @param selpars type of selection to be in the algorithm.
#' @param convcrit convergence criteria for the algorithm containing: 
#' \it{type} it is the type of convergence criterion; \it{pars} it is the value
#' the type of convergence criterion. 
#' @param lower and upper limits of the distribution. Must be finite.
#' 

ExpDE<-function(popsize = 40, mutpars = list(name = "rand", f = 0.2),
                recpars = list(name = "bin"),
                selpars = list(name = "standard"),
                convcrit = list(types = c("niter", "stab"),
                           pars = list(niter = 500, nstab = 5)),
                probpars = list(name = "myfun",
                                lim_inf = -5.12, lim_sup = 5.12, opt = 3)){
    # Differential evolution - a simple and efficient adaptive scheme for global optimization over continuous spaces. 
    # Storn e Price(1995) Rainer Storn e Kenneth Price.
    # Technical report, International Computer Science Institute
    
    # Federal University of Minas Gerais
    # Department of Electrical Engineering
    # Moises Botelho and Felipe Campelo, Ph.D.
    
    #Generation the initial population 
    X <- population(popsize, probpars$opt, probpars$lim_inf, probpars$lim_sup)
    #Evaluate the initial population
    J <- do.call(probpars$name, args = list(X))
  
    if(convcrit$types[1] == "niter"){
      #Generation  
      for (t in 1:convcrit$pars$niter){
    #while (t <= ngen){
      #Mutation
      M <- do.call(mutpars$name, args = list(X, mutpars$f))
      #Recombination  
      U <- do.call(recpars$name, args = list(X, M))
      # Evaluate U
      G <- do.call(probpars$name, args = list(U))
      #Selection  
      next.pop <- do.call(selpars$name, args = list(U, G, X, J))
      
      X <- next.pop$X
      J <- next.pop$J
      
    }
    }
    return (X)
  }