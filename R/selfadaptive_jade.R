#' JADE parameter adaptation method for DE
#' 
#' Implements the JADE method with external archive for the ExpDE 
#' framework
#' 
#' @section JADE Parameters:
#' The \code{adapars} parameter contains all parameters required for parameter adaptation. 
#' \code{selfadaptive_jade()} understands the following 
#' fields in \code{adapars}:
#' \itemize{
#'    \item \code{mu.cr} : mean of normal distribution for calculating crossover
#'                         probability (CRi).
#'    \item \code{mu.F} : location parameter of the Cauchy distribution for calculating
#'                        mutation factors (Fi). 
#'    \item \code{c} : control the rate of parameter adaptation.
#'    \item \code{CRi} : crossover probability of each individual in population.
#'    \item \code{Fi} : mutation factor od each individual in population.
#'    \item \code{A} : optional archive, set of archived inferior solutions of the 
#'                      population.
#'    \item \code{S.cr} : set of all successful crossover probabilitie's CRi's at a 
#'                        geneation.
#'    \item \code{S.F} : set of all successful mutation factors in generation.
#' }
#' 
#' @section Warning:
#'  The implementation of JADE considers the mutation strategy "DE/current-to-p-best"
#'  and recombination strategy "DE/bin".
#'  The name of the operators for recombination, mutation and selection must be 
#'  the corresponding to the JADE method in the definition of parameters. \code{
#'  mutation_jade, recombination_bin and selection_jade}.
#'  
#'
#' @section References:
#' J. Zhang, A.C. Sanderson, 
#' "JADE: Adaptive differential evolution with optional external archive". 
#' IEEE Transactions on Evolutionary Computation 13:945-958, 2009
#'
#' 
#' @param L list with all parameters for ExpDE framework
#' 
#' @return List \code{L} containing all updated parameters.
#' 
#' @export
selfadaptative_jade <- function(L) {
  # ========== Error catching and default value definitions
  
  assertthat::assert_that("popsize"                %in% names(L),
                          "probpars"               %in% names(L))
  
  assertthat::assert_that(identical(L$mutpars$name, "mutation_jade") & 
                          identical(L$selpars$name, "selection_jade")&
                          identical(L$recpars$name, "recombination_bin_jade"))
  
  assertthat::assert_that(is.numeric(L$adapars$c),
                          is.numeric(L$adapars$mu.cr),
                          is.numeric(L$adapars$mu.F))
  # ========== 
  
  mean.cr <- L$adapars$mu.cr
  mean.F  <- L$adapars$mu.F
  
  #Auxiliar function: Lehmer mean
  meanL <- function(SF) {
    return((sum(SF ^ 2)) / (sum(SF)))
  }
  
  #Check if it is not the first iteration to update the mean.cr and mean.F
  if(identical(L$t, 1)) {
    L$files <- list(A    = c(),
                    S.cr = c(), 
                    S.F  = c())
  } else {
    #Verify the files
    assertthat::assert_that("A"    %in% names(L$files),
                            "S.cr" %in% names(L$files),
                            "S.F"  %in% names(L$files))
    
    #Randomly remove solutions from A so that |A| <= NP
    if(length(L$files$A) > length(L$X)) {
      
      rv        <- sample(1:length(L$files$A),
                          length(L$files$A) - length(L$X), 
                          replace = FALSE)
      L$files$A <- L$files$A[-rv]
    }
    
    
    #Updating of values
    c <- L$adapars$c
    
    mean.cr <- (1 - c) * mean.cr + c * mean(L$files$S.cr)
    mean.F  <- (1 - c) * mean.F  + c * meanL(L$files$S.F)
  }
  
  
  #Get problem dimension
  popsize  <- L$popsize
  probpars <- L$probpars 
  prob.dim <- length(probpars$xmax)
  
  L$adapars$CRi <- matrix(stats::rnorm(n    = popsize * prob.dim, 
                                       mean = mean.cr, 
                                       sd   = 0.1), 
                          nrow = popsize)
  
  L$adapars$Fi <- matrix(stats::rcauchy(n        = popsize * prob.dim, 
                                        location = mean.F, 
                                        scale    = 0.1), 
                         nrow = popsize)
  
  L$adapars$mu.cr <- mean.cr
  L$adapars$mu.F  <- mean.F
  
  
  return(L)
}