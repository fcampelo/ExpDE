bin <- function(pop, m) {
 #Decision matrix to cross
 delta <- matrix(round(runif(n = dim(pop)[1]*dim(pop)[2],
                                min = 0, max = 1)), 
                 nrow = dim(pop)[1])
 
  #Individuals Crossing 
  U <- m * delta + pop * (1-delta)
 
  return(U)
}