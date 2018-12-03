perform_mutation <- function(L){
  mutpars <- L$mutpars
  
  # ========== Error catching and default value definitions
  if (!("nvecs" %in% names(mutpars))) mutpars$nvecs <- 1
  
  assertthat::assert_that(is.matrix(L$X), is.numeric(L$X),
                          assertthat::is.count(mutpars$nvecs),
                          assertthat::has_name(mutpars, "f"),
                          is.numeric(mutpars$f))
  
  if (length(mutpars$f) == 1) mutpars$f <- rep(mutpars$f, 
                                               mutpars$nvecs)
  # ==========
  
  L$M <-       do.call(mutpars$name,
                       args = list(X       = L$X,
                                   J       = L$J,
                                   mutpars = mutpars))
  L$mutpars <- mutpars
  return(L)
}