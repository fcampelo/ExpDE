#' Get experimental parameters
#' 
#' Get parameters for practicing the design and running of a factorial experiment 
#' using ExpDE as a data-generating process.
#' 
#' @param ID student ID
#' 
#' @return List object with experimental parameters that need to be used in the 
#' design and execution of the experiment.
#' 

get_experimental_parameters_activity2 <- function(ID){
  
  # ========== Error catching 
  assertthat::assert_that(assertthat::is.count(ID))
  # ==========
  
  # set PRNG seed
  set.seed(ID)
  
  output <- list()
  
  # Minimally relevant effect sizes
  output$mres   <- sample(x = .05 * (5:15), size = 1)
  
  # Desired significance
  output$alpha <- sample(0.01 * (1:5), size = 1)
  
  # Desired power level
  output$desired.power <- sample(0.75 + 0.05 * (0:3), size = 1)
  
  # number of repetitions for each combination of levels
  output$nruns <- sample(c(10, 15, 25, 30, 50), size = 1)
  
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
  
  mutidx <- sample.int(n       = length(mutlist), 
                       size    = 3, 
                       replace = FALSE)
  output$mutation_type <- mutlist[mutidx]
  mut_print <- paste0(mutations[mutidx], "Use mutpars = output$mutation_type[[", seq_along(mutidx), "]]")
  
  
  ## Recombination operators
  # As list:
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
    list(alias = "1-point", name = "recombination_onepoint"), 
    list(alias = "p-Best", name = "recombination_pbest", 
         cr = signif(stats::runif(1, min = .25, max = .75), 2)), 
    list(alias = "Wright", name = "recombination_wright"))
  
  
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
    paste0("Recomb: Onepoint(). "), 
    paste0("Recomb: pBest(CR = ", reclist[[11]]$cr, "). "), 
    paste0("Recomb: Wright(). "))
  
  recidx <- sample.int(n       = length(recombinations), 
                       size    = sample(c(3,5), 1), 
                       replace = FALSE)
  
  output$recomb_type <- reclist[recidx]
  rec_print <- paste0(recombinations[recidx], "Use recpars = output$recomb_type[[", seq_along(recidx), "]]")
  
  message("Desired statistical parameters for your experimental design:")
  message("Significance level (alpha): alpha = ", output$alpha)
  message("Minimally relevant effect size (MRES): d* = ", output$mres)
  message("Desired statistical power for MRES (desired power): (1-beta)* = ", output$desired.power)
  message("Number of repeated runs for each combination of levels: nruns = ", output$nruns)
  message("============================================================")
  message("Factors and levels for your experiment:")
  message("Factor (1) : mutation type")
  message("Levels to investigate:")
  print(mut_print, quote = FALSE, width = 10)
  message("Factor (2) : recombination type")
  message("Levels to investigate:")
  print(rec_print, quote = FALSE, width = 10)
  
  invisible(output)
  
}
