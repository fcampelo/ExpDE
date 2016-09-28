rastrigin <- function(Pop,...){
  apply(X = Pop,
        FUN = rastfun,
        MARGIN = 1)
}

rastfun <- function(x){
  n <- length(x);
  A <- 10
  f <- A * n;
  
  for (k in 1:n) {
    f <- f + (x[k] ^ 2 - A * cos(2 * pi * x[k]));
  }

  return (f);
}
