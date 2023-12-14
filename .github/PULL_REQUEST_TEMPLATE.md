**What changes are proposed in this pull request?**


**Reference GitHub issue associated with pull request.** _e.g., 'closes #1'_


--------------------------------------------------------------------------------

Reviewer Checklist (if item does not apply, mark is as complete)

- [ ] Ensure all package dependencies are installed: `devtools::install_dev_deps()`
- [ ] PR branch has pulled the most recent updates from master branch: `usethis::pr_merge_main()`
- [ ] If a bug was fixed, a unit test was added.
- [ ] Run `pkgdown::build_site()`. Check the R console for errors, and review the rendered website.
- [ ] Code coverage is suitable for any new functions/features: `devtools::test_coverage()`
- [ ] `usethis::use_spell_check()` runs with no spelling errors in documentation

When the branch is ready to be merged into master:
- [ ] Update `NEWS.md` with the changes from this pull request under the heading "`# cards (development version)`". If there is an issue associated with the pull request, reference it in parentheses at the end update (see `NEWS.md` for examples).
- [ ] Increment the version number using `usethis::use_version(which = "dev")`
- [ ] Run `usethis::use_spell_check()` again
- [ ] Approve Pull Request
- [ ] Merge the PR. Please use "Squash and merge".
