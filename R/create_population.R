#' Create population
#' 
#' Create new population for the ExpDE framework
#' 
#' The detailed description comes here...
#' 
#' @importFrom ExpDE
#' 
#' @return A matrix containing the population for the ExpDE

create_population <- function(popsize,      # population size
                              probpars)     # list of named problem parameters
{
  #Generate population of individuals within the standardized space x \in (0,1)
  
  # Include error catching:
  # - probpars must have fields "lim_sup" e "lim_inf"
  # - popsize must be equal or greater than 3
  
  # get problem dimension
  prob.dim <- length(probpars$lim_sup)
  
  return (matrix(runif(n = popsize * probpars$prob.dim), 
                 nrow = popsize))
}