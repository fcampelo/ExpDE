selecao <- function(U, G, X, J){
  
  sel.vec       <- G <= J
  X[sel.vec, ]  <- U[sel.vec, ]
  J[sel.vec]    <- G[sel.vec]
  
  return(list(X = X, J = J))
}