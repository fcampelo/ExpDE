#' Perform mutation
#' 
#' Implements the control of mutation functions for the ExpDE framework
#' 
#' @section Mutation Parameters:
#' The \code{mutpars} parameter contains all parameters required to define the 
#' mutation. 
#' 
#' @param L list with all parameters for ExpDE framework
#' 
#' @return List \code{L} containing all updated parameters, including
#' the mutated population \code{M}
#' 
#' @author Bruna Queiroz (\email{bsqueiroz98@@gmail.com})
#' 
#' @export

perform_mutation <- function(L){
  mutpars <- L$mutpars
  
  # ========== Error catching and default value definitions
  if (!("nvecs" %in% names(mutpars))) mutpars$nvecs <- 1
  
  assertthat::assert_that("X" %in% names(L),
                          "J" %in% names(L))
  
  assertthat::assert_that(is.matrix(L$X), is.numeric(L$X),
                          assertthat::is.count(mutpars$nvecs))
  
  if(!isTRUE(L$adapars$use)) {
    assertthat::assert_that(assertthat::has_name(mutpars, "f"), 
                            is.numeric(mutpars$f))
    
    if (length(mutpars$f) == 1) mutpars$f <- rep(mutpars$f, 
                                                 mutpars$nvecs)
  }
  
  # ==========
  
  L$M <-       do.call(mutpars$name,
                       args = list(L       = L,
                                   mutpars = mutpars))
  L$mutpars <- mutpars
  return(L)
}