#' Recombination operators available
#' 
#' List all available recombination operators in the ExpDE package
#' 
#' @return Character vector with the names of all recombination operator 
#'         routines
#' 
#' @export

recombination_operators <- function(){
  c("arith", 
  "bin",
  "blxAlpha",
  "blxAlphaBeta",
  "eigen",
  "exp",
  "flat",
  "geo",
  "lbga",
  "linear",
  "mmax",
  "npoint",
  "none",
  "onepoint",
  "pbest",
  "sbx",
  "wright")
}