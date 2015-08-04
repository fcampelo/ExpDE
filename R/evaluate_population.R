evaluate_population <- function (probpars, X){
  
  # Denormalize population
  LL <- matrix(rep(lim_inf, nrow(X)),
               ncol = ncol(X),
               byrow = TRUE)
  UL <- matrix(rep(lim_sup, nrow(X)),
               ncol = ncol(X),
               byrow = TRUE)
  X <- LL + X*(UL-LL)
  
  # Evaluate each candidate solution
  Z <- apply(X,
             1,
             probpars$name,
             probpars)
  
  # Return evaluated values
  return (Z)
  
}