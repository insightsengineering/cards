## Test environments
* Ubuntu 18.04 LTS (on github actions), devel, release, oldrel-1, oldrel-2, oldrel-3, oldrel-4
* Windows Server 2019 (on github actions), release
* macOS (on github actions), release
* win-builder devel

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages
 
The new problem occurred in the cardx package. 
I maintain that package and have an updated version ready to send to CRAN as soon as this package is accepted.

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Additional Comments

* Thank you for your time!
