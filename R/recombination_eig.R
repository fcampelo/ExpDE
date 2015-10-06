#' /eig recombination for DE
#' 
#' Implements the "/eig" (eigenvector) recombination for the ExpDE framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_eig()} understands the following fields in 
#' \code{recpars}:
#' \itemize{
#'    \item \code{cr} : component-wise probability of using the value in 
#'                      \code{M}.\cr
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

recombination_eig <- function(X, M, recpars) {

  # ========== Error catching and default value definitions
  if (!("cr" %in% names(recpars))){
    stop("recombination_eig() requires field cr in recpars")
  }
  if (!(0 < recpars$cr & recpars$cr <= 1)) {
    stop("recombination_eig() requires numeric 0 < recpars$cr <= 1")
  }
  if (!identical(dim(X),dim(M))) {
    stop("recombination_eig() requires dim(X) == dim(M)")
  }
  if (!("minchange" %in% names(recpars))){
    recpars$minchange <- TRUE
  }
  # ==========
  
 #eigenvectors
  Q <- eigen(cov(X))$vectors

  #function for calculate eignvectors X
  setfunX <- function(i, Q, PX) {
   z1 <- t(t(Q) %*% PX[i, ])
  }
  
  xt <- t(sapply(1:nrow(X), setfunX, Q = Q, PX = X))
  
  #function for calculate eignvectors M
  setfunM <- function(i, Q, PM) {
    z2 <- t(t(Q) %*% PM[i, ])
  }

  mt <- t(sapply(1:nrow(X), setfunM, Q = Q, PM = M))
 
 
  # Recombination matrix
  R <- randM(X) < recpars$cr
  
  if (recpars$minchange){
    indx    <- which(rowSums(R) == 0)
    cor.mat <- cbind(indx, 
                     sample.int(n       = ncol(X),
                                size    = length(indx), 
                                replace = TRUE))
    R[cor.mat[,1],cor.mat[,2]] <- TRUE
  }
  
  #recombination matrix
  res <- (R*mt + (1 - R)*xt)
  
  #function for calculate result crossover
  setfunR <- function(i, Q, PR) {
    z3 <- t((Q) %*% PR[i, ])
  }
  
  rt <- t(sapply(1:nrow(X), setfunR, Q = Q, PR = res))
  
 
  # Return recombined population
  return(rt)
  
}
