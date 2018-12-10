perform_selection <- function(L) {
  selpars <- L$selpars
  
  # ========== Error catching and default value definitions
  assertthat::assert_that("X" %in% names(L),
                          "J" %in% names(L),
                          "U" %in% names(L),
                          "G" %in% names(L))
  
  assertthat::assert_that(is.matrix(L$X), is.numeric(L$X),
                          is.matrix(L$U), is.numeric(L$U),
                          is.numeric(L$J), is.numeric(L$G),
                          assertthat::are_equal(dim(L$X), dim(L$U)),
                          length(L$J) == nrow(L$X), 
                          length(L$G) == nrow(L$U))
  # ========== 
  
  L$nextpop <- do.call(selpars$name,
                       args = list(X = L$X,
                                   U = L$U, 
                                   J = L$J,
                                   G = L$G))
  
  
  
  L$selpars <- selpars
  return(L)
}