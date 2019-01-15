#' @export
jade <- function(L) {
  # ========== Error catching and default value definitions
  
  assertthat::assert_that("popsize"                %in% names(L),
                          "probpars"               %in% names(L))
  
  assertthat::assert_that(is.numeric(L$adapars$c),
                          is.numeric(L$adapars$mu.cr),
                          is.numeric(L$adapars$mu.F))
  # ========== 
  
  mean.cr <- L$adapars$mu.cr
  mean.F  <- L$adapars$mu.F
  
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
    c <- L$adapars$c
    mean.cr <- (1 - c) * mean.cr + c * mean(L$adapars$S.cr)
    mean.F  <- (1 - c) * mean.F  + c * meanL(L$adapars$S.F)
  }
  
  
  #Get problem dimension
  popsize  <- L$popsize
  probpars <- L$probpars 
  prob.dim <- length(probpars$xmax)
  
  L$adapars$CRi <- matrix(stats::rnorm(n    = popsize * prob.dim, 
                                       mean = mean.cr, 
                                       sd   = 0.1), 
                          nrow = popsize)
  
  L$adapars$Fi <- matrix(stats::rcauchy(n        = popsize * prob.dim, 
                                        location = mean.F, 
                                        scale    = 0.1), 
                         nrow = popsize)
  
  L$adapars$mu.cr <- mean.cr
  L$adapars$mu.F  <- mean.F
  

  return(L)
}