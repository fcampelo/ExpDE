#' p-Best recombination for DE
#' 
#' Implements the "/pbest" (p-Best) recombination for the ExpDE framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_pbest()} understands the following 
#' fields in \code{recpars}:
#' \itemize{
#'    \item \code{cr} : component-wise probability of using the value in 
#'                      \code{M}.\cr
#'                      Accepts numeric value \code{0 < cr <= 1}.
#'    \item \code{maxiter} : cut point for crossover.\cr
#'    Accepts integer value \code{maxiter > 0}
#'}
#'
#' @section References:
#' S.M. Islam, S. Das, S. Ghosh, S. Roy, P.N. Suganthan, "An Adaptive 
#' Differential Evolution Algorithm With Novel Mutation and Crossover 
#' Strategies for Global Numerical Optimization", IEEE. Trans. Systems, Man
#' and Cybernetics - Part B 42(2), 482-500, 2012\cr
#' F. Herrera, M. Lozano, A. M. Sanchez, "A taxonomy for the crossover
#' operator for real-coded genetic algorithms: an experimental study", 
#' International Journal of Intelligent Systems 18(3) 309-338, 2003.
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_pbest <- function(X, M, recpars = list(K = NULL)) {

  # ========== Error catching and default value definitions
  
  # Get access to variables in the calling environment
  env <- parent.frame()
  
  if (!("cr" %in% names(recpars))){
    stop("recombination_pbest() requires field cr in recpars")
  }
  if (!(0 < recpars$cr & recpars$cr <= 1)) {
    stop("recombination_pbest() requires numeric 0 < recpars$cr <= 1")
  }
  if (!("maxiter" %in% names(recpars))) {
    recpars$maxiter <- 100
  }
  if(!(all(recpars$maxiter == floor(recpars$maxiter)))) {
      stop("recombination_pbest() requires an integer value for maxiter")
  }
  if( recpars$maxiter < 1){
    stop("recombination_pbest() maxiter value must be am integer greater than 0")
  }
    
  if (!identical(dim(X), dim(M))) {
    stop("recombination_pbest() requires dim(X) == dim(M)")
  }
  # ==========
  
  # Calculate p-number: number of top-performing vectors to form the
  # recombination pool
  #p = ceil(0.5*np*(1 - (Pop.iter-1)/input.recpar(2)));
  
  p <- ceiling(0.5 * nrow(X) * (1 - (env$t - 1)/recpars$maxiter))
  
  # Assemble recombination pool indices
  indx <- sort.int(env$J, index.return=TRUE)$ix
  indx <- indx[1:p] 
  indx <-indx[sample.int(n = p, size = nrow(X), replace = TRUE)] 
  
  # Matrix of inheritance
  chg <- randM(X) <= recpars$cr
  
  # Perform recombination
  return(chg * M + (!chg) * X[indx,])
  
  

}
