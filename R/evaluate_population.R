evaluate_population <- function (probpars, Pop){
  
  # Denormalize population
  Pop <- denormalize_population(probpars, Pop)
  
  # remove '$name' field from list of arguments
  # and include the points to be evaluated as field "Pop" (or "x" or "X")
  myargs     <- probpars[!(names(probpars) %in% c("name","xmin","xmax"))]
  input_args <- names(formals(get(probpars$name)))
  if("x" %in% input_args){
    myargs$x <- Pop
  } else if ("x" %in% input_args){
    myargs$X <- Pop
  } else if ("Pop" %in% input_args){
    myargs$Pop <- Pop
  } else stop("The objective function must have an input parameter 
              'x' or 'X' or 'Pop', which receives the matrix of points (row vectors) 
              to be evaluated.")
  
  # Evaluate candidate solutions
  Z <- do.call(probpars$name,
               args = myargs)
  
  # Return evaluated values
  return (Z)
}