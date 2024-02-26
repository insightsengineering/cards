## Test environments
* Ubuntu 18.04 LTS (on github actions), devel, release, oldrel-1, oldrel-2, oldrel-3, oldrel-4
* Windows Server 2019 (on github actions), release
* macOS (on github actions), release
* win-builder devel

## R CMD check results

0 errors | 0 warnings | 1 note

  Maintainer: ‘Daniel D. Sjoberg <danield.sjoberg@gmail.com>’
  
  New submission

## Additional Comments

* This is a re-submission of a new release.

* Thank you for the review comments.
  - Unexported functions no longer have examples.
  - The `installed.packages()` function has been removed/replaced.
  - References to software names and APIs in the DESCRIPTION have been removed to highlight other package functionality, therefore, no need to quote them.
  
* There are no additional references describing the methods in this package.

* Thank you for your time!
