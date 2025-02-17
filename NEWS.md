# cards 0.5.0

## New Features and Functions

* Added functions `rename_ard_groups_shift()` and `rename_ard_groups_reverse()` for renaming the grouping variables in the ARD. (#344)

* Added an option to specify the default rounding in the package: `cards.round_type`. See `?cards.options` for details. (#384)

* Added the `print_ard_conditions(condition_type)` argument, which allows users to select to return conditions as messages (the default), or have warnings returned as warnings and errors as errors. (#386)

* Added the `all_ard_group_n(types)` argument to allow separate selection of `groupX` and `groupX_level` columns.

* Added the `tidy_ard_column_order(group_order)` argument that allows users to specify whether the grouping variables are listed in ascending order (the default) or descending order. The output of `ard_strata()` now calls `tidy_ard_column_order(group_order="descending")`.

## Other Updates

* A new article has been added detailing how to create new ARD functions.

* Results are now sorted in a consistent manner, by descending groups and strata. (#342, #326)

## Lifecycle Updates

* Function `label_cards()` has been renamed to `label_round()`, which more clearly communicates that is returns a rounding function.

# cards 0.4.0

## New Features and Functions

* Added functions `as_cards_fn()`, `is_cards_fn()`, and `get_cards_fn_stat_names()`. These functions assist is creating functions with attributes enumerating the expected results.

* Updated `ard_continuous()` and `ard_complex()` to return full ARDs when functions passed are created with `as_cards_fn()`: instead of a single row output, we get a long ARD with rows for each of the expected statistic names. (#316)

* Added function `ard_pairwise()` to ease the calculations of pairwise analyses. (#359)

## Other Updates

* Improved messaging in `print_ard_conditions()` when the calling function is namespaced. (#348)

* Updated print method for `'card'` objects so extraneous columns are never printed by default.

## Lifecycle Changes

* No longer exporting functions `check_pkg_installed()`, `is_pkg_installed()`, `get_min_version_required()`, `get_pkg_dependencies()`. These functions are now internal-only. (#330)

## Bug Fixes

* The `tidy_ard_column_order()` now correctly orders grouping columns when there are 10+ groups. This also corrects an issue in the hierarchical functions where the ordering of the variables matters. (#352)

# cards 0.3.0

## New Features & Updates

* Added functions `ard_stack_hierarchical()` and  `ard_stack_hierarchical_count()` that ease the creation of ARDs for multiple nested or hierarchical structures. (#314)

* Added functions `update_ard_fmt_fn()` and `update_ard_stat_label()` to update an ARD's formatting function and statistic label, respectively. (#253)

* Added `rename_ard_columns(unlist)` argument, which unlists specified columns in the ARD data frame. (#313)

* Added `ard_strata()` function to ease the task of calculating ARDs stratified by one or more other categorical variables. (#273)

* Added functions `mock_continuous()`, `mock_categorical()`, `mock_dichotomous()`, `mock_missing()`, `mock_attributes()` to build ARDs in the absence of a data frame. Where applicable, the formatting functions are set to return `'xx'` or `'xx.x'` to aid in the construction of mock tables or table shells. (#256)

* Added functions for printing results from `eval_capture_conditions()`. Captured conditions can be printed as either errors or messages with `captured_condition_as_error()` and `captured_condition_as_message()`, respectively. (#282)

## Other Updates

* The `ard_hierarchical_count()` function has been updated to match the behavior of `ard_hierarchical()` and results are now only returned for the last column listed in the `variables` arguments, rather than recursively counting all variables.

* Add columns `'fmt_fn'`, `'warning'`, and `'errors'` to `ard_attributes()` output. (#327)

* Add checks for factors with no levels, or any levels that are `NA` into `ard_*` functions (#255) 

* Any rows with `NA` or `NaN` values in the `.by` columns specified in `ard_stack()` are now removed from all calculations. (#320)

# cards 0.2.2

## New Features & Updates

* Converted `ard_total_n()` to an S3 generic and added method `ard_total_n.data.frame()`.

* Added the `bind_ard(.quiet)` argument to suppress messaging. (#299)

* Improved ability of `shuffle_ard()` to populate missing group values where possible. (#306)

* Added `apply_fmt_fn(replace)` argument. Use `replace=FALSE` to retain any previously formatted statistics in the `stat_fmt` column. (#285)

* Added `bind_ard(.distinct)` argument, which can remove non-distinct rows from the ARD across grouping variables, primary variables, context, statistic name and value. (#286)

## Bug Fixes

* Fix in `print_ard_conditions()` when the variables were factors, which did not render properly in `cli::cli_format()`.

* Bug fix in `print_ard_conditions()` and we can now print condition messages that contain curly brace pairs. (#309)

# cards 0.2.1

* Update in `ard_categorical()` to use `base::order()` instead of `dplyr::arrange()`, so the ordering of variables match the results from `base::table()` in some edge cases where sorted order was inconsistent.

* Update in `ard_categorical()` to run `base::table()` output checks against coerced character columns. Previously, we relied on R to perform checks on the type it decided to check against (e.g. when it coerces to a common type). While the initial strategy worked in cases of Base R classes, there were some bespoke classes, such as times from {hms}, where Base R does not coerce as we expected.

* Adding selectors `all_group_n()` and `all_missing_columns()`. (#272, #274)

* Added new function `add_calculated_row()` for adding a new row of calculated statistic(s) that are a function of the other statistics in the ARD. (#275)

# cards 0.2.0

## New Features & Updates

* Converting `ard_*()` functions and other helpers to S3 generics to make them extendable. (#227) 

* Added helper `rename_ard_columns()` for renaming/coalescing group/variable columns. (#213).

* Added new function `ard_total_n()` for calculating the total N in a data frame. (#236)

* Added the `nest_for_ard(include_data)` argument to either include or exclude the subsetted data frames in a list-column in the returned tibble.

* Added `check_ard_structure(column_order, method)` arguments to the function to check for column ordering and whether result contains a `stat_name='method'` row.

* Added the optional `ard_hierarchical(id)` argument. When provided we check for duplicates across the column(s) supplied here. If duplicates are found, the user is warned that the percentages and denominators are not correct. (#214)

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
