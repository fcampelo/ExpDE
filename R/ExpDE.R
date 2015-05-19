#' Differential Evolution Algorithm (DE)
#' 
#' Is a method that optimizes a problem by iteratively trying to 
#' improve a candidate solution with regard to a given measure of quality
#' @param popsize number of population.
#' @param mutpars mutation the parameters list containing:
#'  \it{name} is name of mutation; \it{f} mutation the scaling factor.
#' @param lower and upper limits of the distribution. Must be finite.
#' 
ExpDE <- function(ngen){ 
    # Differential evolution - a simple and efficient adaptive scheme for global optimization over continuous spaces. 
    # Storn e Price(1995) Rainer Storn e Kenneth Price.
    # Technical report, International Computer Science Institute
    
    # Federal University of Minas Gerais
    # Department of Electrical Engineering
    # Moises Botelho and Prof. Felipe Campelo, Ph.D.
    
    # Parametros do Algortimo         
    nvar    <- 3                # Numero de variaveis do individuo
    f       <- 0.2              # Fator de escala da mutacao
    cr      <- 0.9              # Constante de cruzamento
    t       <- 1                # Contador de geracoes
    popsize <- 50
    lim_inf <- -5.12            # Limite inferior do problema de otimizacao
    lim_sup <- 5.12             # Limite superior do problema de otimizacao
    
    
    #Generation the initial population 
    X <- populacao(popsize, nvar, lim_inf, lim_sup)
    J <- myfun(X,lim_inf,lim_sup)
    
    while (t <= ngen){
      #Mutation
      M <- mutacao(X, f)
      #Recombination  
      U <- cruzamento(cr, X, M)
      
      # Evaluate U
      G <- myfun(U,lim_inf,lim_sup)
      
      #Selection  
      next.pop <- selecao(U, G, X, J)
      
      X <- next.pop$X
      J <- next.pop$J
      
      #Generation  
      t <- t + 1
    }
    
    return (X)
  }