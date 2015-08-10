#' /exp recombination for DE
#' One-point recombination (as used in the Simple GA) 
#' 
#' Implements the "/exp" (exponential) recombination for the ExpDE framework
#' 
#' The details (if any) come here...
#' 
#' @section Recombination Parameters:
#' The \code{recpars} parameter contains all parameters required to define the 
#' recombination. \code{recombination_exp()} understands the following fields in 
#' recpars:
#'    - \code{cr} : component-wise probability of using the value in \code{M}.
#'    Accepts numeric values \code{0 < cr <= 1}.
#'
#' @param X population matrix (original)
#' @param M population matrix (mutated) 
#' @param recpars recombination parameters (see \code{Recombination parameters} 
#' for details)
#' 
#' @return Matrix \code{U} containing the recombined population

recombination_exp <- function(X, M, recpars) {

  # Error catching and default value definitions
  if (!("cr" %in% names(recpars))){
    stop("recombination_exp() requires field cr in recpars")
  }
  if (!(0 < recpars$cr & recpars$cr <= 1)) {
    stop("recombination_exp() requires numeric 0 < recpars$cr <= 1")
  }
  if (!identical(dim(X),dim(M))) {
    stop("recombination_exp() requires dim(X) == dim(M)")
  }
          
  # generate multiple random cutpoints
  r <- matrix(rep(t(ceiling((ncol(X)-1)*runif(nrow(X)))), ncol(X)), ncol = ncol(X))
		
  # Matrix of inheritance         	
  chg <- (matrix(rep(1:ncol(X), nrow(X)), ncol = ncol(X))) <= r
     
  if (runif(1) < recpars$cr){ 
     chg <- !chg             # Randomize which vector will donate the variables with the lowermost indexes
	 }
        
  # Recombination matrix
  return(chg*M + (!chg)*X) 

}
