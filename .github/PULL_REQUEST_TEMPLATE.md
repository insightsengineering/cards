**What changes are proposed in this pull request?**
* Style this entry in a way that can be copied directly into `NEWS.md`. (#<issue number>, @<username>)

Provide more detail here as needed.

**Reference GitHub issue associated with pull request.** _e.g., 'closes #<issue number>'_


--------------------------------------------------------------------------------

Pre-review Checklist (if item does not apply, mark is as complete)
- [ ] **All** GitHub Action workflows pass with a :white_check_mark:
- [ ] PR branch has pulled the most recent updates from master branch: `usethis::pr_merge_main()`
- [ ] If a bug was fixed, a unit test was added.
- [ ] Code coverage is suitable for any new functions/features (generally, 100% coverage for new code): `devtools::test_coverage()`
- [ ] Request a reviewer

Reviewer Checklist (if item does not apply, mark is as complete)

- [ ] If a bug was fixed, a unit test was added.
- [ ] Run `pkgdown::build_site()`. Check the R console for errors, and review the rendered website.
- [ ] Code coverage is suitable for any new functions/features: `devtools::test_coverage()`

When the branch is ready to be merged:
- [ ] Update `NEWS.md` with the changes from this pull request under the heading "`# cards (development version)`". If there is an issue associated with the pull request, reference it in parentheses at the end update (see `NEWS.md` for examples).
- [ ] **All** GitHub Action workflows pass with a :white_check_mark:
- [ ] Approve Pull Request
- [ ] Merge the PR. Please use "Squash and merge" or "Rebase and merge".

_Optional Reverse Dependency Checks_:

- Install `checked` with `pak::pak("Genentech/checked")` or `pak::pak("checked")`
- Check dev versions of `cardx`, `gtsummary`, and `tfrmt` which are in the `ddsjoberg` R Universe

  ```shell
  Rscript -e "options(checked.check_envvars = c(NOT_CRAN = TRUE)); checked::check_rev_deps(path = '.', n = parallel::detectCores() - 2L, repos = c('https://ddsjoberg.r-universe.dev', 'https://cloud.r-project.org'))"
  ```

- Check CRAN reverse dependencies but run tests skipped on CRAN

  ```shell
  Rscript -e "options(checked.check_envvars = c(NOT_CRAN = TRUE)); checked::check_rev_deps(path = '.', n = parallel::detectCores() - 2, repos = 'https://cloud.r-project.org')"
  ```

- Check CRAN reverse dependencies in a CRAN-like environment

  ```shell
  Rscript -e "options(checked.check_envvars = c(NOT_CRAN = FALSE), checked.check_build_args = '--as-cran'); checked::check_rev_deps(path = '.', n = parallel::detectCores() - 2, repos = 'https://cloud.r-project.org')"
  ```
