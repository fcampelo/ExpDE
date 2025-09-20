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

get_experimental_parameters <- function(ID){
  
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
  
  # Mutation operators
  mutations <- list(
    mutation_best = paste0('mutpars = list(name = "mutation_best", f = ', signif(stats::runif(1, min = .1, max = 2), 2), ")"),
    mutation_rand = paste0('mutpars = list(name = "mutation_rand", f = ', signif(stats::runif(1, min = .1, max = 2), 2), ")"),
    mutation_mean = paste0('mutpars = list(name = "mutation_mean", f = ', signif(stats::runif(1, min = .1, max = 2), 2), ")"),
    mutation_wgi = paste0('mutpars = list(name = "mutation_wgi", f = ', signif(stats::runif(1, min = .1, max = 2), 2), ")")
  )
  
  mutidx <- sample.int(n       = length(mutations), 
                       size    = 2, 
                       replace = FALSE)
  output$mutation <- list(mutations[[mutidx[1]]],
                          mutations[[mutidx[2]]])
  
  
  recombinations = list(
    recombination_arith = paste0('recpars = list(name = "recombination_arith")'), 
    recombination_bin = paste0('recpars = list(name = "recombination_bin", cr = ', 
                               signif(stats::runif(1, min = .25, max = .75), 2), ")"), 
    recombination_blxAlphaBeta = paste0('recpars = list(name = "recombination_blxAlphaBeta", alpha = ', 
                                        signif(stats::runif(1, .1, .4), 2),
                                        ", beta = ",
                                        signif(stats::runif(1, .1, .4), 2), ")"), 
    recombination_exp = paste0('recpars = list(name = "recombination_exp", cr = ',
                               signif(stats::runif(1, min = .25, max = .75), 2), ")"), 
    recombination_geo = paste0('recpars = list(name = "recombination_geo", alpha = ',
                               signif(stats::runif(1, min = .25, max = .75), 2), ")"), 
    recombination_lbga = paste0('recpars = list(name = "recombination_lbga")'), 
    recombination_linear = paste0('recpars = list(name = "recombination_linear")'), 
    recombination_mmax = paste0('recpars = list(name = "recombination_mmax")'), 
    recombination_npoint = paste0('recpars = list(name = "recombination_npoint")'), 
    recombination_onepoint = paste0('recpars = list(name = "recombination_onepoint")'), 
    recombination_pbest = paste0('recpars = list(name = "recombination_pbest", cr = ',
                                 signif(stats::runif(1, min = .25, max = .75), 2),")"), 
    recombination_wright = paste0('recpars = list(name = "recombination_wright")'))
  
  recidx <- sample.int(n       = length(recombinations), 
                       size    = sample(c(3,4), 1), 
                       replace = FALSE)
  
  output$recombination <- vector("list", length(recidx))
  for (i in seq_along(recidx)) {
    output$recombination[[i]] <- recombinations[[recidx[i]]]
  }
  
  message("Desired statistical parameters for your experimental design:")
  message("Significance level (alpha): ", output$alpha)
  message("Minimally relevant effect size (MRES): ", output$mres)
  message("Desired statistical power at MRES (desired power): ", output$desired.power)
  message("============================================================")
  message("Factors and levels for your experiment:")
  message("Factor (1) : mutation type")
  message("Levels to investigate:")
  for(i in seq_along(output$mutation)){
    print(unname(output$mutation[[i]]), quote = FALSE)
  }
  
  message("Factor (2) : recombination type")
  message("Levels to investigate:")
  
  for(i in seq_along(output$recombination)){
    print(unname(output$recombination[[i]]), quote = FALSE)
  }
  
  invisible(output)
  
}
