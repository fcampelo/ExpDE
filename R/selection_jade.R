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
  
  #verify ????
  #error
  S.cr <- c(files$S.cr, CRi[sel.vec])
  S.F  <- c(files$S.F,  adapars$Fi[sel.vec])
  A    <- c(files$A,    X[!sel.vec,])
  
  L$files <- list(A    = A,
                  S.cr = S.cr,
                  S.F  = S.F)

  return(L)
}