#' Create population
#' 
#' Create new population for the ExpDE framework
#' 
#' The detailed description comes here...
#' 
#' @param popsize population size
#' @param probpars list of named problem parameters (see \code{\link{ExpDE}}).
#' 
#' @return A matrix containing the population for the ExpDE

create_population <- function(popsize,      # population size
                              probpars)     # list of named problem parameters
{
  #Generate population of individuals within the standardized space x \in (0,1)
  
  # Include error catching:
  # - probpars must have fields "xmin" e "xmax"
  # - popsize must be equal or greater than 3
  
  # get problem dimension
  prob.dim <- length(probpars$xmax)
  
  return (matrix(runif(n = popsize * prob.dim), 
                 nrow = popsize))
}