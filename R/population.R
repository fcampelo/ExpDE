population <- function(popsize, probpars){
  #Generation of the population of individuals based on the lower and upper limits

  return (matrix(runif(n = popsize * probpars$opt, min =  probpars$lim_inf, 
                       max =  probpars$lim_sup), 
                 nrow = popsize, byrow = T)
  )
}