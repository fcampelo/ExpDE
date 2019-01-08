jade <- function(L, adapars) {
  # ========== Error catching and default value definitions
  
  assertthat::assert_that("popsize"  %in% names(L),
                          "probpars" %in% names(L))
  
  assertthat::assert_that(is.numeric(adapars$c),
                          is.numeric(adapars$mu.cr),
                          is.numeric(adapars$mu.F))
  # ========== 
  
  mean.cr <- adapars$mu.cr
  mean.F  <- adapars$mu.F
  
  #Auxiliar function: Lehmer mean
  meanL <- function(SF) {
    return((sum(SF ^ 2)) / (sum(SF)))
  }
  
  #Check if it is not the first iteration to update the mean.cr and mean.F
  if(identical(L$t, 1)) {
    L$files <- list(A    = c(),
                    S.cr = c(), 
                    S.F  = c())
  } else {
    #Verify the files
    assertthat::assert_that("A"    %in% names(L$files),
                            "S.cr" %in% names(L$files),
                            "S.F"  %in% names(L$files))
    
    #Randomly remove solutions from A so that |A| <= NP
    if(length(L$files$A) > length(L$X)) {
      
      rv        <- sample(1:length(L$files$A),
                          length(L$files$A) - length(L$X), 
                          replace = FALSE)
      L$files$A <- L$files$A[-rv]
    }
    
    
    #Updating of values
    mean.cr <- (1 - c) * mean.cr + c * mean(adapars$S.cr)
    mean.F  <- (1 - c) * mean.F  + c * meanL(adapars$S.F)
  }
  
  
  #Get problem dimension
  popsize  <- L$popsize
  probpars <- L$probpars 
  prob.dim <- length(probpars$xmax)
  
  adapars$CRi <- matrix(stats::rnorm(n    = popsize * prob.dim, 
                                     mean = mean.cr, 
                                     sd   = 0.1), 
                        nrow = popsize)
  
  adapars$Fi <- matrix(stats::rcauchy(n        = popsize * prob.dim, 
                                      location = mean.F, 
                                      scale    = 0.1), 
                       nrow = popsize)
  
  adapars$mu.cr <- mean.cr
  adapars$mu.F  <- mean.F
  
  return(adapars)
}