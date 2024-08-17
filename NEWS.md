# cards 0.2.1

* Update in `ard_categorical()` to use `base::order()` instead of `dplyr::arrange()`, so the ordering of variables match the results from `base::table()` in some edge cases where sorted order was inconsistent.

* Update in `ard_categorical()` to run `base::table()` output checks against coerced character columns. Previously, we relied on R to perform checks on the type it decided to check against (e.g. when it coerces to a common type). While the initial strategy worked in cases of Base R classes, there were some bespoke classes, such as times from {hms}, where Base R does not coerce as we expected.

# cards 0.2.0

## New Features & Updates

* Converting `ard_*()` functions and other helpers to S3 generics to make them extendable. (#227) 

* Added helper `rename_ard_columns()` for renaming/coalescing group/variable columns. (#213).

* Added new function `ard_total_n()` for calculating the total N in a data frame. (#236)

* Added the `nest_for_ard(include_data)` argument to either include or exclude the subsetted data frames in a list-column in the returned tibble.

* Added `check_ard_structure(column_order, method)` arguments to the function to check for column ordering and whether result contains a `stat_name='method'` row.

* Added the optional `ard_heirarchicial(id)` argument. When provided we check for duplicates across the column(s) supplied here. If duplicates are found, the user is warned that the percentages and denominators are not correct. (#214)

* Improved messaging in `check_pkg_installed()` that incorporates the calling function name in the case of an error. (#205)

* Updated `is_pkg_installed()` and `check_pkg_installed()` to allow checks for more than package at a time. The `get_min_version_required()` function has also been updated to return a tibble instead of a list with attributes. (#201)

* Styling from the {cli} package are now removed from errors and warnings when they are captured with `eval_capture_conditions()`. Styling is removed with `cli::ansi_strip()`. (#129)

## Bug Fixes

* Bug fix in `ard_stack()` when calls to functions were namespaced. (#242)

* The `print_ard_conditions()` function has been updated to no longer error out if the ARD object does not have `"error"` or `"warning"` columns. (#240)

* Bug fix in `shuffle_ard()` where factors were coerced to integers instead of their labels. (#232)

## Lifecycle Changes

* Corrected order that `ard_categorical` (strata) columns would appear in the ARD results. Previously, they appeared in the order they appeared in the original data, and now they are sorted properly. (#221)

* The API for `ard_continuous(statistic)` and `ard_missing(statistic)` arguments has been updated. Previously, the RHS of these argument's passed lists would be either `continuous_summary_fns()` and `missing_summary_fns()`. Now these arguments accept simple character vectors of the statistic names. For example, `ard_categorical(statistic = everything() ~ c("n", "p", "N"))` and `ard_missing(statistic = everything() ~ c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss"))`. (#223) 

* Updated `ard_stack()` to return `n`, `p`, and `N` for the `by` variable when specified. Previously, it only returned `N` which is the same for all levels of the by variable. (#219)

* Bug fix where `ard_stack(by)` argument was not passed to `ard_missing()` when `ard_stack(.missing=TRUE)`. (#244)

* The `ard_stack(by)` argument has been renamed to `".by"` and its location moved to after the dots inputs, e.g. `ard_stack(..., .by)`. (#243)

* A messaging overhaul to utilize the scripts in `https://github.com/ddsjoberg/standalone/blob/main/R/standalone-cli_call_env.R`. This allows clear error messaging across functions and packages. (#42)
  - The `print_ard_conditions(call)`, `check_list_elements(env)`, `cards_select(.call)` arguments have been removed.

# cards 0.1.0

* Initial release.
