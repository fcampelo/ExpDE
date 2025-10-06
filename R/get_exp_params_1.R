#' Get experimental parameters
#' 
#' Get parameters for practicing the design and running of a comparative experiment 
#' using ExpDE as a data-generating process.
#' 
#' @param ID a unique positive integer (e.g., a student ID)
#' @param echo Boolean, should the function print its output?
#' 
#' @return List object with experimental parameters that need to be used for the 
#' design of the experiment.
#' 
#' @export

get_exp_params_1 <- function(ID, echo = TRUE){
  
  # ========== Error catching 
  assertthat::assert_that(assertthat::is.count(ID))
  # ==========
  
  # Save the PRNG state
  oldseed <- .Random.seed
  
  # set PRNG seed
  set.seed(ID)
  
  output <- list()
  
  # Minimally relevant effect sizes
  output$mres   <- sample(x = .05 * (10:20), size = 1)
  
  # Desired significance
  output$alpha <- sample(0.01 * (1:5), size = 1)
  
  # Desired power level
  output$desired.power <- sample(0.75 + 0.05 * (0:3), size = 1)
  
  # number of repetitions for each combination of levels
  output$nruns <- sample(c(10, 15, 25, 30, 50), size = 1)
  
  if(echo){ 
    message("Desired statistical parameters for your experimental design:")
    message("============================================================")
    message("Significance level (alpha): alpha = ", output$alpha)
    message("Minimally relevant effect size (MRES): d* = ", output$mres)
    message("Desired statistical power for MRES (desired power): (1-beta)* = ", output$desired.power)
    message("Number of repeated runs for each method on each problem: nruns = ", output$nruns)
    message("============================================================")
  }
  # Restore the PRNG to where it was
  .Random.seed <<- oldseed
  
  invisible(output)
  
}


