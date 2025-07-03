## Test environments
* Ubuntu 18.04 LTS (on github actions), devel, release, oldrel-1, oldrel-2, oldrel-3, oldrel-4
* Windows Server 2019 (on github actions), release
* macOS (on github actions), release
* win-builder devel

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 2 new problems
 * We failed to check 0 packages

The gtsummary and cardx packages failed reverse dependency checks.
I maintain both of these packages and thes failures are expected, and I will resubmit them to CRAN as soon as this package is accepted.
 
## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Additional Comments

* Thank you for your time!
