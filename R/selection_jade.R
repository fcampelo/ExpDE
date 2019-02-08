#' Standard selection adapted to the JADE function
#' 
#' Implements the standard selection (greedy) for the ExpDE framework
#' 
#' @section Files:
#' Implements the update of parameters \code{A, S.cr and S.F} to the 
#' routine JADE.
#'         
#' @section X: 
#' Population matrix (original).
#' @section U: 
#' Population matrix (recombined). 
#' @section J: 
#' Performance vector for population \code{X}.
#' @section G: 
#' Performance vector for population \code{U}.
#' 
#' @param L list with all parameters for ExpDE framework
#' 
#' @return List \code{L} containing all updated parameters, including
#' list object containing the selected population (\code{Xsel}) and 
#' its corresponding performance values (\code{Jsel}).
#' 
#' @export
selection_jade <- function(L){
  # ========== Error catching and default value definitions
  assertthat::assert_that("files" %in% names(L),
                          "adapars" %in% names(L))
  
  assertthat::assert_that("A"    %in% names(L$files),
                          "S.cr" %in% names(L$files),
                          "S.F"  %in% names(L$files))
  # ========== 
  X <- L$X
  U <- L$U
  J <- L$J
  G <- L$G
  
  sel.vec       <- (G <= J)
  X[sel.vec, ]  <- U[sel.vec, ]
  J[sel.vec]    <- G[sel.vec]
  
  L$nextpop <- list(Xsel = X, 
                    Jsel = J) 
  
  
  #Complete files
  adapars <- L$adapars
  files   <- L$files
  CRi     <- adapars$CRi
  
  #Verify
  S.cr <- c(files$S.cr, CRi[sel.vec])
  S.F  <- c(files$S.F,  adapars$Fi[sel.vec])
  A    <- c(files$A,    X[!sel.vec,]) 
  
  L$files <- list(A    = A,
                  S.cr = S.cr,
                  S.F  = S.F)

  return(L)
}