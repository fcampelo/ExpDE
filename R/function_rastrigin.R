rastrigin <- function(Pop, multiplier = 1, ...){
  apply(X = Pop,
        FUN = rastfun,
        MARGIN = 1,
        multiplier = multiplier)
}

rastfun <- function(x, multiplier = 1){
  n <- length(x);
  A <- 10
  f <- A * n;
  
  for (k in 1:n) {
    f <- f + (x[k] ^ 2 - A * cos(2 * pi * x[k]));
  }

  return (multiplier * f);
}
