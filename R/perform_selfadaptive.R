#' Perform self-adaptive methods 
#' 
#' Implements the control of self adaptive functions for the ExpDE framework
#' 
#' @section Self adaptive parameters:
#' The \code{adapars} parameter contains all parameters required to define the 
#' use and control of self adaptive variables. 
#' 
#' @param L list with all parameters for ExpDE framework
#' 
#' @return List \code{L} containing all updated parameters
#' 
#' @author Bruna Queiroz (\email{bsqueiroz98@@gmail.com})
#' 
#' @export
perform_selfadaptive <- function(L) {
  
  if(isTRUE(L$adapars$use)) {
    
    # ========== Error catching and default value definitions
    assertthat::assert_that("name" %in% names(L$adapars))
    # ========== 
    
    L <- do.call(L$adapars$name,
                args = list(L))
    } 
  
  return(L)
  
}