#' Generate methods for testing 
#' 
#' Generate a number of random DE configurations.
#' 
#' @param ID a unique positive integer (e.g., a student ID)
#' @param echo Boolean, should the function print its output?
#' 
#' @return List object with a number of configurations to use with \code{\link{ExpDE}}. Alternatively, 
#' the method names returned by this function can be passed directly to \code{\link{ExpDE2}}.
#' 
#' @export

gen_methods <- function(ID, echo = TRUE){
  
  # ========== Error catching 
  assertthat::assert_that(assertthat::is.count(ID))
  # ==========
  
  # Save the PRNG state
  oldseed <- .Random.seed
  
  # set PRNG seed
  set.seed(ID)
  
  # Generate configurations for the experiment:
  output <- vector("list", sample(4:6, 1))
  
  for (i in seq_along(output)){
    output[[i]]$MethodName <- paste0("DE.", ID, ".", LETTERS[i])
    output[[i]]$popsize    <- sample(c(50, 100, 150, 200), 1)
    output[[i]]$mutpars    <- gen_mutpars()
    output[[i]]$recpars    <- gen_recpars()
    output[[i]]$selpars    <- list(name = "selection_standard")
    output[[i]]$stopcrit   <- list(names = c("stop_maxeval", "stop_maxiter"),
                                   maxevals = 2000,
                                   maxiter = 2000 / output[[i]]$popsize)
    output[[i]]$showpars   <- list(show.iters = "none")
  }
  
  if(echo){ 
    message("============================================================")
    message("Your experiment will compare ", length(output), " algorithm configurations")
    message("Your selected methods are named:")
    for (i in seq_along(output)){
      message(output[[i]]$MethodName)
    }
    message("Each method can be passed by name as the argument method.name of function ExpDE2()")
    message("If you want to inspect the specificities of each method, check the output list.")
  }

  # Restore the PRNG to where it was
  .Random.seed <<- oldseed
  
  invisible(output)
  
}


