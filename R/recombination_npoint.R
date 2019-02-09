#' n-point recombination for DE
#' 
#' Implements the "/npoint" (n-point) recombination for the ExpDE (as used in 
#' the Simple GA).
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_npoint()} understands the following 
#' fields in \code{recpars}:
#' \itemize{
#'    \item \code{N} : cut number points for crossover.\cr
#'    Accepts integer value \code{0 <= N < n}, where \code{n} is the 
#'    dimension of the problem; Use \code{N = 0} or \code{N = NULL} for randomly 
#'    choosing a number of cut points.\cr
#'    Defaults to \code{NULL}.
#'}
#'
#' @section References:
#' L.J. Eshelman, R.A. Caruana, J.D. Schaffer (1989), "Biases in the crossover 
#' landscape. In: Proceedings of the Third International Conference on Genetic 
#' Algorithms, pp. 10-19, San Francisco, CA, USA.
#' 
#' @section X:
#' Population matrix (original).
#' @section M: 
#' Population matrix (mutated).
#' 
#' @param L list with all parameters for ExpDE framework 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_npoint <- function(L, recpars = list(N = NULL)) {
  X       = L$X
  M       = L$M
  
  # ========== Error catching and default value definitions
  assertthat::assert_that(is.null(recpars$N) || 
                            (assertthat::is.count(recpars$N) && is_within(recpars$N, 0, ncol(X) - 1)))
  # ========== 
  
  # Define the number of cut points for each recombination pair.
  if (is.null(recpars$N) || recpars$N == 0) {
    recpars$N <- sample.int(n       = ncol(X) - 1, 
                            size    = nrow(X),
                            replace = TRUE)  
  } else {
    recpars$N <- rep(x = recpars$N, 
                     times = nrow(X))
  }
  
  
  # Generate random cut points
  cutlist <-  lapply(recpars$N, 
                     FUN = function(x,n){
                       sort(sample.int(n       = n,
                                       size    = x,
                                       replace = FALSE))
                     },
                     n = ncol(X) - 1)
  
  makemask <- function(cuts, x){
    m <- matrix(1:ncol(x),
                nrow  = length(cuts),
                ncol  = ncol(x),
                byrow = TRUE)
    m <- colSums(m > matrix(cuts,
                            nrow  = nrow(m),
                            ncol  = ncol(m),
                            byrow = FALSE))
    return(m %% 2 == 0)
  }
  
  # Assemble recombination matrix
  R <- t(vapply(X   = cutlist,
                FUN = makemask,
                FUN.VALUE = logical(ncol(X)),
                x = X))
  
  # Randomize which population will donate the variables with the lowermost 
  # indexes
  if (stats::runif(1) < 0.5){ 
    R <- !R
  }
  
  # Return recombined population
  return(R * M + (!R) * X)
}
