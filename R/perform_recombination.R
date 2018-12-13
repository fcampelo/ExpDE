perform_recombination <- function(L){
  recpars <- L$recpars
  
  # ========== Error catching and default value definitions
  assertthat::assert_that("X" %in% names(L),
                          "M" %in% names(L))
  
  assertthat::assert_that(is.matrix(L$X), is.numeric(L$X),
                          is.matrix(L$M), is.numeric(L$M),
                          assertthat::are_equal(dim(L$X), dim(L$M)))
  
  # ==========  
  
  # Check if uses the self-adaptive parameters
  adapars <- L$adapars
  
  #temporary -> change to verify if use jade and use recombination_bin_jade
  if(isTRUE(adapars$use)) {
    if(identical(adapars$name, "jade")) {
      recpars$name <- "recombination_bin_jade"
    }
  }
  
  L$U <- do.call(recpars$name,
                 args = list(L,
                             recpars = recpars))

  L$recpars <- recpars
  return(L)
}