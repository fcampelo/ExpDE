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
  
  # Check if uses the self-adaptive parameters
  if(isTRUE(L$adapars$use)) {
    assertthat::assert_that(identical(L$adapars$name, "selfadaptative_jade") &
                            identical(L$selpars$name, "selection_jade"))
  }
  # ========== 
  
  L <- do.call(selpars$name,
               args = list(L))
  
  
  
  L$selpars <- selpars
  return(L)
}