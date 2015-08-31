#' Blend Alpha Beta recombination for DE
#' 
#' Implements the "/blxAlphaBeta" (Blend Alpha Beta) recombination for the ExpDE 
#' framework
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_blxAlpha()} understands the following 
#' fields in recpars:
#'    - \code{alpha} : extrapolation parameter.
#'    Accepts real value \code{0 <= alpha <= 0.75}. Since the internal coding of
#'    ExpDE standardizes variables to the interval 0 <= x <= 1. This parameter 
#'    increases the interval in which the individual is generated.     
#'    
#'    - \code{beta} : extrapolation parameter.
#'    Accepts real value \code{0 <= alpha <= 0.25}. Since the internal coding of
#'    ExpDE standardizes variables to the interval 0 <= x <= 1, the use of high
#'    values of \code{alpha} reduces the interval at which the individual is generated.
#'
#' @section References:
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

recombination_blxAlphaBeta <- function(X, M, recpars, ...) {

  # ========== Error catching and default value definitions
  if (!("alpha" %in% names(recpars))){
    stop("recombination_blxAlphaBeta() requires field alpha in recpars")
  }
  if (!("beta" %in% names(recpars))){
    stop("recombination_blxAlphaBeta() requires field beta in recpars")
  }
  if(!is.numeric(recpars$alpha)){
    stop("recombination_blxAlphaBeta() requires a numeric rectpars$alpha")
  }
  
  if(!is.numeric(recpars$beta)){
    stop("recombination_blxAlphaBeta() requires a numeric rectpars$beta")
  }
  if(!(0 <= recpars$alpha & recpars$alpha <= 0.75)){
    stop("recombination_blxAlphaBeta() requires 0 <= recpars$alpha <= 0.75")
  }
  if(!(0 <= recpars$beta & recpars$beta <= 0.25)){
    stop("recombination_blxAlphaBeta() requires 0 <= recpars$beta <= 0.25")
  }
  if (!identical(dim(X), dim(M))) {
    stop("recombination_blxAphaBeta() requires dim(X) == dim(M)")
  }
  # ==========
  
  #Evaluate of the M population solutions
  popMAval <- evaluate_population(probpars, M)

  
  #Evaluate of the X population solutions
  popXAval <- matrix(evaluate_population(probpars, X), ncol = ncol(X), nrow = nrow(X))
  

  #Get index of element in vector
  index <-popXAval <= popMAval
 
  #Obs: FALTA INCREMENTAR A NFE (AVALIÇÃO DE FUNÇÃO)
  
  Cmin <- pmin(X, M)
  Cmax <- pmax(X, M)
  I    <- Cmax - Cmin
  
  C1 <- X * index + M * !index
  
  C2 <- M * index + X * !index
  
  S <- (Cmin == C1) * recpars$alpha + (Cmin == C2) * recpars$beta
 
 
  # Return recombined population 
  return (Cmin - S * I + (1 + recpars$alpha
                         + recpars$beta) * randM(X) * I)
  
}
