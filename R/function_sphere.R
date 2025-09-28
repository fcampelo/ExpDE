sphere <- function(x, multiplier = 1){
  if (is.matrix(x)) {
    f <- sqrt(rowSums(x^2))
  } else f <- sqrt(sum(x^2))
  
  return(multiplier * f)
}