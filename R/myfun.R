myfun <- function (X,lim_inf,lim_sup){
  
  makefun <- function(pos, myfun, pop){
      x <-  myfun(pop[pos, ])
  }
  #Applying the selection for each individual of the population 
  Z <- lapply(as.list(1:dim(X)[1]), makefun, rastrigin, X)
  return (matrix(unlist(Z), nrow=dim(X)[1], byrow=T))
  
}