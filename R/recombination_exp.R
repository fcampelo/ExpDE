#' Exponential recombination for DE
#' 
#' Implements the exp recombination (as used in the Simple GA).
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_exp()} understands the following 
#' fields in recpars:
#'    - \code{K} : cut point for crossover. Defaults to NULL.
#'    Accepts integer value \code{0 < K < n}, where \code{n} is the 
#'    dimension of the problem; or \code{K = NULL} for randomly choosing a 
#'    position for each pair of points.
#'
#' @section References:
#' Price, Kenneth, Rainer M. Storn, and Jouni A. Lampinen. 
#' Differential evolution: a practical approach to global optimization. 
#' Springer Science & Business Media, 2006.
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population

recombination_exp <- function(X, M, recpars = list(K = NULL)) {
  
  if (!identical(dim(X),dim(M))) {
    stop("recombination_exp() requires dim(X) == dim(M)")
  }
  
  makeinherits <- function(pos, X, M){
    # Generates a random number size of the individual dimension
    j <- sample.int(n = ncol(X), 
                     size = 1, replace = TRUE)                      
    I <- X
    l <- 0
    
    repeat
    {
      # Child inherits a mutant parameter
      I[j] <- M[j]
      
      # Increment j, modulo ncol(X)
      j <- (j+1) %% ncol(X)
      l <- l + 1
      if (runif(1) > recpars$cr || l == ncol(X))
        break
    }
    
    # Return recombined Individual
    return(I) 
    
  }
  
  U <- lapply(X, 
              FUN = makeinherits, 
              X = X, 
              M = M)
  
  return(matrix(data  = unlist(U), nrow = nrow(X), ncol= ncol(X), byrow = T))
}
