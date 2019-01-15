#' @export
# /current-to-pbest mutation for Adaptative DE

mutation_jade <- function(L, mutpars){
  X <- L$X
  J <- L$J
  
  # ========== Error catching and default value definitions
  
  assertthat::assert_that(is.numeric(mutpars$p), 
                          is_within(mutpars$p, 0, nrow(X), strict = TRUE))
  
  if (is_within(mutpars$p, 0, 1, strict = TRUE)){
    mutpars$p <- ceiling(mutpars$p * nrow(X))
  }
  # ==========
  
  # Indices to the p-best vectors
  ibest <- order(J)[1:mutpars$p]
  
  # Matrix indices for mutation (x_{pbest}, x_{r1}, x_{r2})
  R <- mapply(FUN     = function(x, i, ibest) {c(sample.int(ibest, 1), i, 
                                                 sample.int(x, 2, replace = FALSE))},
              x       = rep(nrow(X), 
                            times = nrow(X)),
              i       = 1:nrow(X),
              MoreArgs = list(ibest),
              SIMPLIFY = FALSE)
  
  
  # Auxiliary function: make a single mutation
  pbestmut <- function(pos, Pop, file, f){
    diffs <- matrix(pos,
                    ncol  = 2,
                    byrow = TRUE)
    Pop2  <- sample(x       = c(Pop, file), 
                    size    = length(Pop), 
                    replace = FALSE)
    Pop2  <- matrix(data = Pop2,
                    nrow = nrow(Pop),
                    ncol = ncol(Pop))
    
    return(Pop[pos[2], ] + 
             colSums(f * (Pop[diffs[, 1], ] - Pop2[diffs[, 2], ]))) #erro para multiplicar
  }
  #To do:
  #how to ensure that Pop[diffs[, 1], ] - Pop2[diffs[, 2], ] are different
  # Apply mutation
  M <- lapply(R, 
              FUN    = pbestmut, 
              Pop    = X,
              file   = L$files$A,
              f      = mutpars$f)
  
  return(matrix(data  = unlist(M), 
                nrow  = nrow(X), 
                byrow = TRUE))
}
