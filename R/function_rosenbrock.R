rosenbrock <- function(Pop, multiplier = 1, ...){
  apply(X = Pop,
        FUN = rosenfun,
        MARGIN = 1,
        multiplier = multiplier)
}

rosenfun <- function(x, multiplier = 1){
  n <- length(x);
  A <- 100
  f <- 0;
  
  for (k in 1:(n-1)) {
    f <- f + A * (x[k+1] - x[k] ^ 2) ^ 2 + (1 - x[k])^2;
  }

  return (multiplier * f);
}
