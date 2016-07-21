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
#'    \item \code{k} : number of cut points for crossover.\cr
#'    Accepts integer value \code{0 <= k < n}, where \code{n} is the 
#'    dimension of the problem; Use \code{k = 0} or \code{k = NULL} for randomly 
#'    choosing a number of cut points.\cr
#'    Defaults to \code{NULL}.
#'}
#'
#' @section References:
#' L.J. Eshelman, R.A. Caruana, J.D. Schaffer (1989), "Biases in the crossover 
#' landscape. In: Proceedings of the Third International Conference on Genetic 
#' Algorithms, pp. 10-19, San Francisco, CA, USA.
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_npoint <- function(X, M, recpars = list(k = NULL)) {
  
  # ========== Error catching and default value definitions
  assertthat::assert_that(is.matrix(X), is.numeric(X),
                          is.matrix(M), is.numeric(M),
                          assertthat::are_equal(dim(X), dim(M)),
                          is.null(recpars$k) || 
                            (assertthat::is.count(recpars$k) && 
                               is_within(recpars$k, 0, ncol(X) - 1)))
  # ========== 
  
  # Define the number of cut points for each recombination pair.
  if (is.null(recpars$k) || recpars$k == 0) {
    recpars$k <- sample.int(n       = ncol(X) - 1, 
                            size    = nrow(X),
                            replace = TRUE)  
  } else {
    recpars$k <- rep(x = recpars$k, 
                     times = nrow(X))
  }
  
  
  # Generate random cut points
  cutlist <-  lapply(recpars$k, 
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
  R <- t(vapply(X         = cutlist,
                FUN       = makemask,
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
