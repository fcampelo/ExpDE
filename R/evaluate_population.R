#' Evaluate DE population
#' 
#' Evaluates the DE population on a given objective function.
#'
#' @param probpars problem parameters (see \code{\link{ExpDE}} for details).
#' @param Pop population matrix (each row is a candidate solution, normalized 
#' to the [0, 1] interval,)
#' 
#' @return numeric vector (with length \code{nrow(Pop)}) containing the function 
#' values of each point in the population.
#' 
#' @export
evaluate_population <- function (probpars, Pop){
  
  # ========== Error catching and default value definitions
  assertthat::assert_that(is.matrix(Pop), is.numeric(Pop),
                          all(assertthat::has_name(probpars, c("name", 
                                                               "xmin", 
                                                               "xmax"))),
                          all(probpars$xmin < probpars$xmax))
  if(assertthat::has_name(probpars, "matrixEval")){
    assertthat::assert_that(probpars$matrixEval %in% c("vector", 
                                                       "rowMatrix",
                                                       "colMatrix"))
  } else probpars$matrixEval <- "rowMatrix"
  # ========== 
  
  # Denormalize population
  Pop <- denormalize_population(probpars, Pop)
  
  # remove '$name' field from list of arguments
  # and include the points to be evaluated as field "Pop" (or "x" or "X")
  args       <- probpars[!(names(probpars) %in% c("name","xmin",
                                                  "xmax","matrixEval"))]
  input_args <- names(formals(get(probpars$name)))
  if("x" %in% input_args){
    myargs <- c(list(x = Pop), args)
  } else if ("X" %in% input_args){
    myargs <- c(list(X = Pop), args)
  } else if ("Pop" %in% input_args){
    myargs <- c(list(Pop = Pop), args)
  } else stop("The objective function must have an input parameter 
              'x' or 'X' or 'Pop', which receives the matrix of points (row vectors) 
              to be evaluated.")
  
  # Evaluate candidate solutions
  if(probpars$matrixEval == "rowMatrix"){
    Z <- do.call(probpars$name,
                 args = myargs)
  } else if (probpars$matrixEval == "colMatrix"){
    myargs[[1]] <- t(myargs[[1]])
  } else {
    Z <- numeric(nrow(myargs[[1]]))
    for (i in seq_along(Z)){
      pargs <- myargs
      pargs[[1]] <- as.vector(myargs[[1]][i, ])
      Z[i] <- do.call(probpars$name,
                      args = pargs)
    }
  }
  
  # Return evaluated values
  return (Z)
}