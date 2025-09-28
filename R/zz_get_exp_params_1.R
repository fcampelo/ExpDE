#' Get experimental parameters
#' 
#' Get parameters for practicing the design and running of a factorial experiment 
#' using ExpDE as a data-generating process.
#' 
#' @param ID a unique positive integer (e.g., a student ID)
#' 
#' @return List object with experimental parameters that need to be used in the 
#' design and execution of the experiment.
#' 

get_exp_params_1 <- function(ID){
  
  # ========== Error catching 
  assertthat::assert_that(assertthat::is.count(ID))
  # ==========
  
  # Save the PRNG state
  oldseed <- .Random.seed
  
  # set PRNG seed
  set.seed(ID)
  
  output <- list()
  
  # Minimally relevant effect sizes
  output$mres   <- sample(x = .05 * (5:15), size = 1)
  
  # Desired significance
  output$alpha <- sample(0.01 * (1:5), size = 1)
  
  # Desired power level
  output$desired.power <- sample(0.75 + 0.05 * (0:3), size = 1)
  
  # number of repetitions for each combination of levels
  output$nruns <- sample(c(10, 15, 25, 30, 50), size = 1)
  
  # Generate configurations for the experiment:
  output$methods <- vector("list", sample(4:6, 1))
  
  for (i in seq_along(output$methods)){
    output$methods[[i]]$MethodName <- paste0("DE(", ID, ".", LETTERS[i], ")")
    output$methods[[i]]$popsize    <- sample(c(50, 100, 150, 200), 1)
    output$methods[[i]]$mutpars    <- gen_mutpars()
    output$methods[[i]]$recpars    <- gen_recpars()
    output$methods[[i]]$selpars    <- list(name = "standard")
    output$methods[[i]]$stopcrit   <- list(name = "stop_maxeval", maxevals = 10000)
    output$methods[[i]]$showpars   <- list(show.iters = "none")
  }
    
  message("Desired statistical parameters for your experimental design:")
  message("Significance level (alpha): alpha = ", output$alpha)
  message("Minimally relevant effect size (MRES): d* = ", output$mres)
  message("Desired statistical power for MRES (desired power): (1-beta)* = ", output$desired.power)
  message("Number of repeated runs for each method on each problem: nruns = ", output$nruns)
  message("============================================================")
  message("Your experiment will compare ", length(output$methods), " algorithm configurations")
  message("The selected methods are:")
  for (i in seq_along(output$methods)){
    message("- ", output$methods[[i]]$MethodName)
  }
  message("(These methods can be passed as names,\nor run using the setups provided in output$methods)")
  
  class(output) <- c("ExpDEconfigs")
  
  # Restore the PRNG to where it was
  .Random.seed <<- oldseed
  
  invisible(output)
  
}


