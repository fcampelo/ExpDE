#' Standard selection for DE
#' 
#' Implements the standard selection (greedy) for the ExpDE framework
#' 
#'        
#' @param X population matrix (original)
#' @param U population matrix (recombined) 
#' @param J performance vector for population \code{X}
#' @param G performance vector for population \code{U}
#' 
#' @return list object containing the selected population (\code{Xsel}) and 
#' its corresponding performance values (\code{Jsel}).
#' 
#' @export

selection_standard <- function(X, U, J, G){
  
  # ========== Error catching and default value definitions
  assertthat::assert_that(is.matrix(X), is.numeric(X),
                          is.matrix(U), is.numeric(U),
                          is.numeric(J), is.numeric(G),
                          assertthat::are_equal(dim(X), dim(U)),
                          length(J) == nrow(X), 
                          length(G) == nrow(U))
  # ========== 
  
  sel.vec       <- (G <= J)
  X[sel.vec, ]  <- U[sel.vec, ]
  J[sel.vec]    <- G[sel.vec]
  
  return(list(Xsel = X, 
              Jsel = J))
}