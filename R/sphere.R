sphere <- function(x){
  if (is.matrix(x)) {
    sqrt(rowSums(x^2))
  } else sqrt(sum(x^2))
}