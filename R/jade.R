jade <- function(L, adapars) {
  # ========== Error catching and default value definitions
  
  assertthat::assert_that("popsize"  %in% names(L),
                          "probpars" %in% names(L))
  
  assertthat::assert_that(is.numeric(adapars$c),
                          is.numeric(adapars$mu.cr),
                          is.numeric(adapars$mu.F))
  # ========== 
  
  #Check if it is not the first iteration to update the mu.cr and mu.F
  #if first iteration A <- c() S.cr <- c(), S.F <- c()
  if(identical(L$t, 1)) {
    L$files <- list(A    = c(),
                    S.cr = c(), 
                    S.F  = c())
  } else {
    
    
  }
  
  
  #Get problem dimension
  popsize  <- L$popsize
  probpars <- L$probpars 
  prob.dim <- length(probpars$xmax)
  
  mean.cr <- adapars$mu.cr
  mean.F  <- adapars$mu.F
  
  
  adapars$CRi <- matrix(stats::rnorm(n    = popsize * prob.dim, 
                                     mean = mean.cr, 
                                     sd   = 0.1), 
                        nrow = popsize)
  
  adapars$Fi <- matrix(stats::rcauchy(n        = popsize * prob.dim, 
                                      location = mean.F, 
                                      scale    = 0.1), 
                       nrow = popsize)
  
  return(adapars)
}