recombination_bin_jade <- function(L, recpars) {
  X       = L$X
  M       = L$M
  
  # ========== Error catching and default value definitions
  if (!assertthat::has_name(recpars, "minchange")) recpars$minchange <- TRUE
  
  assertthat::assert_that(assertthat::has_name(L$adapars, "CRi"),
                          is.numeric(adapars$CRi),
                          is_within(adapars$CRi, 0, 1),
                          assertthat::is.flag(recpars$minchange))
  # ==========
  
  # Recombination matrix
  R <- randM(X) < adapars$CRi
  
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