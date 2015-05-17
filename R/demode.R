#' lfsdslfjl
demode <- function(ngen){
  # Differential evolution - a simple and efficient adaptive scheme for global optimization over continuous spaces. 
  # Storn e Price(1995) Rainer Storn e Kenneth Price.
  # Technical report, International Computer Science Institute
  
  # Federal University of Minas Gerais
  # Department of Electrical Engineering
  # Moises de Matos Botelho
  
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
  # F <- myfun(X,lim_inf,lim_sup)
  
  while (t <= ngen){
    #Mutation
      M <- mutacao(X, f)
    #Recombination  
      U <- cruzamento(cr, X, M)
    
    # Evaluate U
    # G <- myfun(U,lim_inf,lim_sup)
    
    #Selection  
    # next.pop <- selecao(U, X, F, G)
    # X <- next.pop$X
    # F <- next.pop$F
    
    #Generation  
      t <- t + 1
  }
  
  return (X)
}