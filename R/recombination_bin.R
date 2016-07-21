#' /bin recombination for DE
#' 
#' Implements the "/bin" (binomial) recombination for the ExpDE framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_bin()} understands the following fields in 
#' \code{recpars}:
#' \itemize{
#'    \item \code{rho} : component-wise probability of using the value in 
#'                       \code{M}.\cr
#'                      Accepts numeric value \code{0 < cr <= 1}.
#'    \item \code{minchange} : logical flag to force each new candidate solution 
#'                             to inherit at least one component from its 
#'                             mutated 'parent'.\cr
#'                             Defaults to TRUE
#' }
#'
#' @section References:
#' K. Price, R.M. Storn, J.A. Lampinen, "Differential Evolution: A 
#' Practical Approach to Global Optimization", Springer 2005
#' 
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_bin <- function(X, M, recpars) {

  # ========== Error catching and default value definitions
  if (!assertthat::has_name(recpars, "minchange")) recpars$minchange <- TRUE
  
  assertthat::assert_that(is.matrix(X), is.numeric(X),
                          is.matrix(M), is.numeric(M),
                          assertthat::are_equal(dim(X), dim(M)),
                          assertthat::has_name(recpars, "rho"),
                          is.numeric(recpars$rho),
                          is_within(recpars$rho, 0, 1),
                          assertthat::is.flag(recpars$minchange))
  # ==========
  
  # Recombination matrix
  R <- randM(X) < recpars$rho
  
  if (recpars$minchange){
    indx    <- which(rowSums(R) == 0)
    cor.mat <- cbind(indx, 
                     sample.int(n       = ncol(X),
                                size    = length(indx), 
                                replace = TRUE))
    R[cor.mat[, 1], cor.mat[, 2]] <- TRUE
  }
  
  # Return recombined population
  return(R * M + (1 - R) * X)
}
