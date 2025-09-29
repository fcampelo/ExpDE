## Changes and notes
* Maintenance update:
    * Removed `mutation_current_to_pbest()`. 
    * Minor fixes to documentation
    * Added functions to generate random  experiments using different DE 
    setups and randomly generated test problems, to allow ExpDE to be used as a 
    tool to learn best practices in experimental design for the comparison of 
    optimisation algorithms.

## Test environments

### Testing via Github actions
- macOS 15.6.1, R 4.5.1
- Ubuntu 24.04.3 LTS, R (devel)
- Ubuntu 24.04.3 LTS, R 4.4.3 
- Ubuntu 24.04.3 LTS, R 4.5.1 
- MS Windows Server 2025 10.0.26100, R 4.5.1

### Local testing
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)
R CMD check results - ExpDE 0.2.0
Duration: 16.7s
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
R CMD check succeeded

