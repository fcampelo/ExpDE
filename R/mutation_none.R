#' NULL mutation for DE
#' 
#' Implements the "/none" mutation (i.e., no mutation performed) for the 
#' ExpDE framework
#' 
#' @section Mutation Parameters:
#' The \code{mutpars} parameter contains all parameters required to define the 
#' mutation. \code{mutation_none()} requires no fields in this parameter.
#' 
#' @param X population matrix
#' @param mutpars mutation parameters (see \code{Mutation parameters} for details)
#' 
#' @return @return The same matrix \code{X} used as an input.
#' 
#' @export

mutation_none <- function(X, J, mutpars){
  return(X)
}