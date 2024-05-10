# cards 0.1.0.9023

* Bug fix where `ard_stack(by)` argument was not passed to `ard_missing()` when `ard_stack(.missing=TRUE)`. (#244)

* The `print_ard_conditions()` function has been updated to no longer error out if the ARD object does not have `"error"` or `"warning"` columns. (#240)

* Added the optional `ard_heirarchicial(id)` argument. When provided we check for duplicates across the column(s) supplied here. If duplicates are found, the user is warned that the percentages and denominators are not correct. (#214)

* Corrected order that `ard_categorical` (strata) columns would appear in the ARD results. Previously, they appeared in the order they appeared in the original data, and now they are sorted properly. (#221)

* Updated `ard_stack()` to return `n`, `p`, and `N` for the `by` variable when specified. Previously, it only returned `N` which is the same for all levels of the by variable. (#219)

* Improved messaging in `check_pkg_installed()` that incorporates the calling function name in the case of an error. (#205)

* Styling from the {cli} package are now removed from errors and warnings when they are captured with `eval_capture_conditions()`. Styling is removed with `cli::ansi_strip()`. (#129)

* Updated `is_pkg_installed()` and `check_pkg_installed()` to allow checks for more than package at a time. The `get_min_version_required()` function has also been updated to return a tibble instead of a list with attributes. (#201)

* A messaging overhaul to utilize the scripts in `https://github.com/ddsjoberg/standalone/blob/main/R/standalone-cli_call_env.R`. This allows clear error messaging across functions and packages. (#42)
  - As a part of this change, the `is_pkg_installed()` and `check_pkg_installed()` function have been migrated to a standalone script `https://github.com/ddsjoberg/standalone/blob/main/R/standalone-check_pkg_installed.R` and are no longer exported by {cards}.
  - The `print_ard_conditions(call)`, `check_list_elements(env)`, `cards_select(.call)` arguments have been removed.

* Bug fix in `shuffle_ard()` where factors were coerced to integers instead of their labels. (#232)

# cards 0.1.0

* Initial release.
