population <- function(popsize, nvar, lim_inf, lim_sup){
  #Generation of the population of individuals based on the lower and upper limits
  return (matrix(runif(n = popsize * nvar, min = lim_inf, max = lim_sup), 
                 nrow = popsize, byrow = T)
  )
}