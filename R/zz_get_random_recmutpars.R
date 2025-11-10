gen_recpars <- function(idx = NULL){
  reclist = list(
    list(alias = "Arithmetic", name = "recombination_arith"), 
    list(alias = "Binomial", name = "recombination_bin", 
         cr = signif(stats::runif(1, min = .25, max = .75), 2)), 
    list(alias = "BLX", name = "recombination_blxAlphaBeta", 
         alpha = signif(stats::runif(1, .1, .4), 2),
         beta = signif(stats::runif(1, .1, .4), 2)), 
    list(alias = "Exponential", name = "recombination_exp", 
         cr = signif(stats::runif(1, min = .25, max = .75), 2)), 
    list(alias = "Geometric", name = "recombination_geo", 
         alpha = signif(stats::runif(1, min = .25, max = .75), 2)), 
    list(alias = "LBGA", name = "recombination_lbga"), 
    list(alias = "Linear", name = "recombination_linear"), 
    list(alias = "Nmax", name = "recombination_mmax"), 
    list(alias = "N-point", name = "recombination_npoint"), 
    list(alias = "p-Best", name = "recombination_pbest", 
         cr = signif(stats::runif(1, min = .25, max = .75), 2)))
  
  # As text:
  recombinations = c(
    paste0("Recomb: Arith(). "), 
    paste0("Recomb: Bin(CR = ", reclist[[2]]$cr, '). '), 
    paste0("Recomb: BLX(alpha = ", reclist[[3]]$alpha, ', beta = ', reclist[[3]]$beta, "). "), 
    paste0("Recomb: Exp(CR = ", reclist[[4]]$cr, "). "), 
    paste0("Recomb: Geo(alpha = ", reclist[[5]]$alpha, "). "), 
    paste0("Recomb: LBGA(). "), 
    paste0("Recomb: Linear(). "), 
    paste0("Recomb: Nmax(). "),
    paste0("Recomb: Npoint(). "), 
    paste0("Recomb: pBest(CR = ", reclist[[10]]$cr, "). "))
  
  if(is.null(idx)) idx <- sample.int(length(reclist), 1)
  
  retlist <- reclist[[idx]]
  retlist$txt <- recombinations[idx]
  
  return(retlist)
  
}

gen_mutpars <- function(idx = NULL){
  # Mutation operators
  # As list
  mutlist <- list(
    list(alias = "Best", 
         name = "mutation_best", 
         f = signif(stats::runif(1, min = .1, max = 2), 2)),
    list(alias = "Rand", 
         name = "mutation_rand", 
         f = signif(stats::runif(1, min = .1, max = 2), 2)),
    list(alias = "Mean", 
         name = "mutation_mean", 
         f = signif(stats::runif(1, min = .1, max = 2), 2)),
    list(alias = "WGI", 
         name = "mutation_wgi", 
         f = signif(stats::runif(1, min = .1, max = 2), 2))
  )
  
  # As text:
  mutations <- c(
    paste0("Mutation: Best(F = ", mutlist[[1]]$f, "). "),
    paste0("Mutation: Rand(F = ", mutlist[[2]]$f, "). "),
    paste0("Mutation: Mean(F = ", mutlist[[3]]$f, "). "),
    paste0("Mutation: WGI(F = ", mutlist[[4]]$f, "). ")
  )
  
  if(is.null(idx)) idx <- sample.int(length(mutlist), 1)
  
  retlist <- mutlist[[idx]]
  retlist$txt <- mutations[idx]
  
  return(retlist)
  
}