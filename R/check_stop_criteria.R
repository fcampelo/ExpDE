#' Stop criteria for DE
#' 
#' Implements different stop criteria for the ExpDE framework
#' 
#' @section Warning:
#' This routine accesses the parent environment used in the main function 
#' \code{ExpDE()}, which means that changes made in the variables 
#' contained in \code{env} WILL change the original values. DO NOT change 
#' anything unless you're absolutely sure of what you're doing.
#' 
#' @return logical flag indicating whether any stop condition has been reached.
#' @export
#' 
check_stop_criteria <- function(L){
  
  # ========== Error catching and default value definitions
  assertthat::assert_that("stopcrit" %in% names(L),
                          "names" %in% names(L$stopcrit))

  crits <- L$stopcrit$names
  
  assertthat::assert_that(("stop_maxiter" %in% crits && "maxiter" %in% names(L$stopcrit)) ||
                          ("stop_maxeval" %in% crits && "maxevals" %in% names(L$stopcrit)))
  # ==========
  
  keep.running <- TRUE
  
  #for (crit in crits){
  #  keep.running <- keep.running * !(do.call(crit,
  #                                           args = list()))
  #}
  
  for (crit in crits){
    keep.running <- keep.running * !(do.call(crit,
                                             args = list(L)))
  }
  
  return(as.logical(keep.running))
}

# Stop criterion: maximum number of iterations

##stop_maxiter <- function(env = parent.frame(n = 2)){
##  return(env$t >= env$stopcrit$maxiter)
##}
stop_maxiter <- function(L){
  return(L$t >= L$stopcrit$maxiter)
}


# Stop criterion: maximum number of objective function calls
stop_maxeval <- function(L){
  #return(env$nfe >= env$stopcrit$maxevals)
  return(L$nfe >= L$stopcrit$maxevals)
}