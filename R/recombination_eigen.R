#' /eigen recombination for DE
#' 
#' Implements the "/eigen" (eigenvector-based) recombination for the ExpDE 
#' framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_eigen()} understands the following fields 
#' in \code{recpars}:
#' \itemize{
#'    \item \code{othername}: name of the recombination operator to be applied 
#'                            after the projection in the eigenvector basis
#'    \item \code{...} :  parameters required (or optional) to the operator 
#'                        defined by \code{recpars$othername}
#' }
#'
#' @section References:
#' Shu-Mei Guo e Chin-Chang Yang, "Enhancing differential evolution utilizing 
#' eigenvector-based crossover operator", IEEE Transactions on Evolutionary 
#' Computation 19(1):31-49, 2015. 
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population
#' 
#' @export

recombination_eigen <- function(L, recpars) {
  X       = L$X
  M       = L$M

  # ========== Error catching and default value definitions
  assertthat::assert_that(assertthat::has_name(recpars, "othername"),
                          recpars$othername != "recombination_eigen")
  # ==========
  
  # Calculate eigenvectors of the covariance matrix of X
  Q <- eigen(stats::cov(X))$vectors

  # Project vectors in X and M onto basis Q
  Xq <- t(sapply(1:(2*nrow(X)), 
                 FUN = function(i,A,B){t(t(A) %*% B[i, ])}, 
                 A   = Q, 
                 B   = rbind(X, M)))
  
  # Perform base recombination method on projected vectors
  # solution initial for perform_recombination
  # Uq <- do.call(recpars$othername,
  #               args = list(X       = Xq[1:nrow(X), ],
  #                           M       = Xq[(nrow(X) + 1):nrow(Xq), ],
  #                           recpars = recpars))
  
  Ltemp     <- L
  Ltemp$X   <- Xq[1:nrow(X), ]
  Ltemp$M   <- Xq[(nrow(X) + 1):nrow(Xq), ]
  
  Uq <- do.call(recpars$othername,
                args = list(Ltemp,
                            recpars = recpars))
  
  # Project resulting vectors back onto the original basis
  U <- t(sapply(1:nrow(X), 
                FUN = function(i,A,B){t(A %*% B[i, ])}, 
                A   = Q, 
                B   = Uq))
 
  # Return recombined population
  return(U)
}
