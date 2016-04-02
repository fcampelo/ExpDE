# ExpDE
[![Build Status](https://travis-ci.org/fcampelo/ExpDE.png?branch=master)](https://travis-ci.org/fcampelo/ExpDE) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ExpDE)](http://cran.r-project.org/package=ExpDE)

ExpDE is a modular implementation of the Differential Evolution metaheuristic, which aims at providing a platform for the experimental investigation of the effect of different recombination / mutation / selection operators.

## Installation

Install the package directly from CRAN using:
```R
install.packages("ExpDE")
```

To get the latest version from Github, use:

```R
# install.packages("devtools")
devtools::install_github("fcampelo/ExpDE")
```

## How to use

Full usage instructions and examples can be found in the documentation of `ExpDE::ExpDE()`. Type `?ExpDE` to check it.

## Available Operators

### Recombination
- Arithmetic (*recombination_arith*)
- Binomial (*recombination_bin*)
- BLX-&alpha; (*recombination_blxAlpha*)
- BLX-&alpha;-&beta; (*recombination_blxAlphaBeta*)
- Eigenvector-based (*recombination_eigen*)
- Exponential (*recombination_exp*)
- Flat (*recombination_flat*)
- Geometric (*recombination_geo*)
- Linear BGA (*recombination_lbga*)
- Linear (*recombination_linear*)
- M-max (*recombination_mmax*)
- N-point (*recombination_npoint*)
- One-point (*recombination_onepoint*)
- pbest (*recombination_pbest*)
- SBX (*recombination_sbx*)
- Wright (*recombination_wright*)   

### Mutation
- Rand (*mutation_rand*)
- Best (*mutation_best*)
- Weighted global intermediate (*mutation_wgi*)

### Selection
- Standard DE (*selection_standard*)

### Stop criteria
- Maximum number of iterations (*stop_maxiter*)
- Maximum number of function evaluations (*stop_maxeval*)

## Example: DE/best/1/sbx on the shifted sphere problem for N = 10
```R
popsize  <- 200
recpars  <- list(name = "recombination_sbx", eta = 10)
mutpars  <- list(name = "mutation_best", f = 0.6, nvecs = 1)
selpars  <- list(name = "selection_standard")
stopcrit <- list(names = "stop_maxeval", maxevals = 100000)
probpars <- list(name  = "sphere", xmin = rep(2, 10), xmax = rep(20, 10))
seed <- 1234
showpars <- list(show.iters = "numbers", showevery = 10)
out <- ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars, seed, showpars)
```

If you find any bugs or has any suggestions for improvement, please feel free to [contact me](fcampelo@ufmg.br).

Cheers,  
Felipe
