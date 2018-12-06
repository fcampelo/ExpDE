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