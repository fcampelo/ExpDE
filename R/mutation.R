rand <- function(pop, f){
  #Matrix indices for mutation
  R <- as.list(
    as.data.frame(sapply(X = rep(dim(pop)[1], dim(pop)[1]),
                         FUN = sample.int,
                         size = dim(pop)[2],
                         replace = FALSE)))
  
  #Function to mutation
  makemut <- function(pos, POP, f){
    x <- POP[pos[1], ] + f * (POP[pos[2], ] - POP[pos[3], ])
  }
  #Applying the mutation for each individual of the public
  M <- lapply(R, makemut, pop, f)
  
  return(matrix(unlist(M), nrow=dim(pop)[1], byrow=T))
}