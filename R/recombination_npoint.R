#' n-point recombination for DE
#' 
#' Implements the n-point recombination (as used in the Simple GA).
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_npoint()} understands the following 
#' fields in \code{recpars}:
#' \itemize{
#'    \item \code{N} : cut number points for crossover.\cr
#'    Accepts integer value \code{0 < N < n}, where \code{n} is the 
#'    dimension of the problem; or \code{N = NULL} for randomly choosing a 
#'    number of cut points.\cr
#'    Defaults to \code{NULL}.
#'}
#'
#' @section References:
#' Eshelman et al.(1989) Larry J. Eshelman, Richard A. Caruana, e J. David Schaffer. Biases in
#' the crossover landscape. Em Proceedings of the Third International Conference on Genetic
#' Algorithms, páginas 10–19, San Francisco, CA, USA. Morgan Kaufmann Publishers Inc.
#' ISBN 1-55860-006-3
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_npoint <- function(X, M, recpars = list(N = NULL)) {

  # ========== Error catching and default value definitions
  if (!("N" %in% names(recpars))) {
    recpars$N <- NULL
  }
  if (!is.null(recpars$N)) {
    if(!(0 < recpars$N & recpars$N < ncol(X))){
      stop("recombination_npoint() requires 0 < recpars$N < n")
    }
    if(!(all(recpars$N == floor(recpars$N)))) {
      stop("recombination_npoint() requires an integer value for N")
    }
  }
  if (!identical(dim(X), dim(M))) {
    stop("recombination_npoint() requires dim(X) == dim(M)")
  }
  # ==========
  
  
  if (is.null(recpars$N)) {
    recpars$N <- sample.int(n = ncol(X)-1, size = 1)  
  }
 
    
  # Generating random points
    cuts <-  sort(sample.int(n = ncol(X)-1, size = recpars$N, replace = FALSE))
  
  # vector index of the individual
    indexs <- rep(1:ncol(X))        
     
  
    setfun <- function(ind, n) {
      z <- ind < n
    }
    
    # Recombination list - using lapply() to apply over multiple indexed objects
    mcut <- lapply(X = cuts, FUN = setfun, n = indexs)
    mcut <- matrix(data = unlist(mcut), ncol = ncol(X))  
  
    # Recombination Matrix
    R <- as.logical(apply(X = mcut, MARGIN = 2, FUN = sum))
    R <- matrix(rep(R, times = nrow(X)),
                nrow  = nrow(X),
                byrow = TRUE)
   
  # Randomize which population will donate the variables with the lowermost 
  # indexes
  if (runif(1) < 0.5){ 
     R <- !R
	 }
        
  # Return recombined population
  return(R * M + (!R) * X) 

}
