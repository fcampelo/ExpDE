# ExpDE 0.2.0
* Maintenance update
* Removed mutation_current_to_pbest (which was essentially unused)
* added function `get_experimental_parameters()`, which can be used to 
    generate a random factorial experiment for students to learn best practices 
    for the statistical comparisons of optimisation heuristics.
* Minor updates to documentation

# ExpDE 0.1.4
* Added mutation operators `mutation_current_to_pbest` (includes special case `current-to-best`), `mutation_mean` and `mutation_wgi`(weighted global intermediate). It is also possible to run the algorithm without using any differential mutation operator (`mutation_none`)
* It is now also possible to run the algorithm without using any recombination operator (`recombination_none`).
* Other minor fixes (invisible for the user)

# ExpDE 0.1.2
* Initial release on CRAN
