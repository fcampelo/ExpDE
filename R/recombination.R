bin <- function(pop, m) {
 #Decision matrix to recombination
 delta <- matrix(round(runif(n = dim(pop)[1]*dim(pop)[2],
                                min = 0, max = 1)), 
                 nrow = dim(pop)[1])
 
  #Individuals Recombination
  U <- m * delta + pop * (1-delta)
 
  return(U)
}
