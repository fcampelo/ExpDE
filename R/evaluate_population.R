evaluate_population <- function (probpars, Pop){
  
  # Denormalize population
  Pop <- denormalize_population(probpars, Pop)
  
  # Evaluate each candidate solution
  Z <- do.call(probpars$name,
               args = list(Pop))
  
  # Return evaluated values
  return (Z)
  
}


# Denormalize population
denormalize_population <- function(probpars, Pop){
  # Denormalize population
  LL <- matrix(rep(probpars$lim_inf, nrow(Pop)),
               ncol = ncol(Pop),
               byrow = TRUE)
  UL <- matrix(rep(probpars$lim_sup, nrow(Pop)),
               ncol = ncol(Pop),
               byrow = TRUE)
  return(LL + Pop*(UL-LL))
}