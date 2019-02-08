#' /bin recombination for adaptive DE
#' 
#' Implements the "/bin" (binomial) recombination with changes to the use
#' of the JADE self-adaptation method
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_bin_jade()} understands the following fields in 
#' \code{recpars}:
#' \itemize{
#'    \item \code{minchange} : logical flag to force each new candidate solution 
#'                             to inherit at least one component from its 
#'                             mutated 'parent'.\cr
#'                             Defaults to TRUE
#' }
#' \code{adapars}:
#' \itemize{
#'    \item \code{CRi}: set of probability of each individual in population
#'                      for using the value in \code{M}. \cr
#' }
#'
#' @section References:
#' K. Price, R.M. Storn, J.A. Lampinen, "Differential Evolution: A 
#' Practical Approach to Global Optimization", Springer 2005
#' 
#' J. Zhang, A.C. Sanderson, 
#' "JADE: Adaptive differential evolution with optional external archive". 
#' IEEE Transactions on Evolutionary Computation 13:945-958, 2009 
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
recombination_bin_jade <- function(L, recpars) {
  X       = L$X
  M       = L$M
  
  # ========== Error catching and default value definitions
  if (!assertthat::has_name(recpars, "minchange")) recpars$minchange <- TRUE
  
  assertthat::assert_that(assertthat::has_name(L$adapars, "CRi"),
                          is.numeric(L$adapars$CRi),
                          assertthat::is.flag(recpars$minchange))
  # ==========
  
  # Recombination matrix
  R <- randM(X) < L$adapars$CRi
  
  if (recpars$minchange){
    indx    <- which(rowSums(R) == 0)
    cor.mat <- cbind(indx, 
                     sample.int(n       = ncol(X),
                                size    = length(indx), 
                                replace = TRUE))
    R[cor.mat[,1],cor.mat[,2]] <- TRUE
  }
  
  # Return recombined population
  return(R*M + (1 - R)*X)
}