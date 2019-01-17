#' Perform recombination
#' 
#' Implements the control of recombination functions for the ExpDE framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. 
#' 
#' @param L list with all parameters for ExpDE framework
#' 
#' @return List \code{L} containing all updated parameters, including
#' the matrix \code{U} containing the recombined population
#' 
#' @author Bruna Queiroz (\email{bsqueiroz98@@gmail.com})
#' 
#' @export

perform_recombination <- function(L){
  recpars <- L$recpars
  
  # ========== Error catching and default value definitions
  assertthat::assert_that("X" %in% names(L),
                          "M" %in% names(L))
  
  assertthat::assert_that(is.matrix(L$X), is.numeric(L$X),
                          is.matrix(L$M), is.numeric(L$M),
                          assertthat::are_equal(dim(L$X), dim(L$M)))
  
  
  # ==========  
  
 
  L$U <- do.call(recpars$name,
                 args = list(L,
                             recpars = recpars))

  L$recpars <- recpars
  return(L)
}