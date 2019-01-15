#' Mutation operators available
#' 
#' List all available mutation operators in the ExpDE package
#' 
#' @return Character vector with the names of all mutation operators
#' 
#' @export

mutation_operators <- function(){
  c("best",
    "current-to-pbest",
    "jade",
    "mean",
    "none",
    "rand",
    "wgi")
}