evaluate_population <- function (probpars, Pop){
  
  # Denormalize population
  Pop <- denormalize_population(probpars, Pop)
  
  # Evaluate candidate solutions
  Z <- do.call(probpars$name,
               args = list(Pop))
  
  # Return evaluated values
  return (Z)
}