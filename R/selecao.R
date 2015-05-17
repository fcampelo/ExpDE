selecao <- function(U, X, F, G){
  
  sel.vec       <- G <= F
  X[sel.vec, ]  <- U[sel.vec, ]
  F[sel.vec]    <- G[sel.vec]
  
  return(list(X = X, F = F))
}