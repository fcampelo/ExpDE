# ExpDE 0.2.1
* Maintenance update: minor bug fix in the new experimental routines
(rare problem cases in `gen_methods`, simplification of the test problems
in `gen_problems`)

# ExpDE 0.2.0
* Maintenance update
* Removed mutation_current_to_pbest (which was essentially unused)
* added functions which to generate random  experiments (aimed at allowing 
  students to practice the statistical comparisons of optimisation heuristics. 
  Check ?ExpDE2 for details.
* Minor updates to documentation

# ExpDE 0.1.4
* Added mutation operators `mutation_current_to_pbest` (includes special case `current-to-best`), `mutation_mean` and `mutation_wgi`(weighted global intermediate). It is also possible to run the algorithm without using any differential mutation operator (`mutation_none`)
* It is now also possible to run the algorithm without using any recombination operator (`recombination_none`).
* Other minor fixes (invisible for the user)

# ExpDE 0.1.2
* Initial release on CRAN
