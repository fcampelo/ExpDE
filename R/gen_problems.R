#' Generate test problems
#' 
#' Get a set of test problems for practicing the design and running of 
#' comparative experiments using ExpDE as a data-generating process.
#' 
#' @param ID a unique positive integer (e.g., a student ID)
#' @param nprobs number of problems to generate
#' @param echo Boolean, should the function print its output?
#' 
#' @return List object with definitions for nprobs test problems.
#' 
#'
#' @export

gen_problems <- function(ID, nprobs,echo=TRUE){
  
  # ========== Error catching 
  assertthat::assert_that(assertthat::is.count(ID),
                          assertthat::is.count(nprobs))
  # ==========
  
  # Save the PRNG state
  oldseed <- .Random.seed
  
  # set PRNG seed
  set.seed(ID)
  
  available_probs <- c("rastrigin", "sphere", "rosenbrock")
  minvals         <- c(-8, -200, -30)
  maxvals         <- c(8, 200, 30)
  maxrange        <- maxvals - minvals
  
  output <- vector("list", nprobs)
  #names(output) <- paste0("Problem(", ID, ".", 1:nprobs, ")")
  
  for(i in seq_along(output)){
    idx <- sample.int(length(available_probs), 1)
    dim <- sample(5:50, 1)
    output[[i]]$ProblemName <- paste0("Prob.", ID, ".", i)
    output[[i]]$name <- available_probs[idx]
    output[[i]]$xmin <- trunc(minvals[idx] + .5 * stats::runif(dim) * maxrange[idx])
    output[[i]]$xmax <- trunc(output[[i]]$xmin + .5 * (1 + stats::runif(dim)) * (maxvals[idx] - output[[i]]$xmin))
  }
  
  if(echo){
    message("Generated ", nprobs, " problem specifications for your experiment.")
    message("Your problem names are:")
    message(paste(sapply(output, \(x)x$ProblemName), collapse = "\n"))
    message("Each problem can be passed by name as the argument problem.name of function ExpDE2()")
    message("If you want to inspect the specificities of each problem, check the output list.")
  }
  
  # Restore the PRNG to where it was
  .Random.seed <<- oldseed
  
  invisible(output)
  
}