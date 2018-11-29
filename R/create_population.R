#' Create population
#' 
#' Creates a new population for the ExpDE framework
#' 
#' @param popsize population size
#' @param probpars list of named problem parameters (see \code{\link{ExpDE}}).
#' 
#' @return A matrix containing the population for the ExpDE
#' 
#' @export

#create_population <- function(popsize,      # population size
#                              probpars)     # list of named problem parameters
create_population <- function(L)
{
  #Generate population of individuals within the standardized space x \in (0,1)
  popsize  <- L$popsize
  probpars <- L$probpars 
  
  # ========== Error catching and default value definitions
  assertthat::assert_that(assertthat::is.count(popsize),
                          is.list(probpars),
                          "xmax" %in% names(probpars))
  # ==========
  
  
  # get problem dimension
  prob.dim <- length(probpars$xmax)
  
  L$X <- matrix(stats::runif(n = popsize * prob.dim), 
                nrow = popsize)
  return (L)
}