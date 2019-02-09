#' Stop criteria for DE
#' 
#' Implements different stop criteria for the ExpDE framework
#' 
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
  
  for (crit in crits){
    keep.running <- keep.running * !(do.call(crit,
                                             args = list(L)))
  }
  
  return(as.logical(keep.running))
}

# Stop criterion: maximum number of iterations
stop_maxiter <- function(L){
  return(L$t >= L$stopcrit$maxiter)
}


# Stop criterion: maximum number of objective function calls
stop_maxeval <- function(L){
  return(L$nfe >= L$stopcrit$maxevals)
}