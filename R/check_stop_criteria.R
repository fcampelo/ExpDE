#' Stop criteria for DE
#' 
#' Implements different stop criteria for the ExpDE framework
#' 
#' The details (if any) come here...
#' 
#' @section Warning:
#' This routine accesses the parent environment used in the main function 
#' \code{ExpDE()}, which means that changes made in the variables 
#' contained in \code{env} WILL change the original values. DO NOT change 
#' anything unless you're absolutely sure of what you're doing.
#' 
#' @param env environment from which to inherit the variable values. Do not 
#' change.
#' 
#' @return logical flag indicating whether any stop condition has been reached.
#' @export
check_stop_criteria <- function(env = parent.frame()){
  
  crits <- env$stopcrit$names
  keep.running <- TRUE
  
  for (crit in crits){
    keep.running <- keep.running * !(do.call(crit,
                                             args = list()))
  }
  
  return(as.logical(keep.running))
}

# Stop criterion: maximum number of iterations
stop_maxiter <- function(env = parent.frame(n = 2)){
  return(env$t >= env$stopcrit$maxiter)
}

stop_maxeval <- function(env = parent.frame(n = 2)){
  return(env$nfe >= env$stopcrit$maxevals)
}