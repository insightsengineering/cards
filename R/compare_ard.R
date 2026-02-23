#' Compare ARDs
#'
#' @description
#' `compare_ard()` compares columns of two ARDs row-by-row using a shared set
#' of key columns. Rows where the column values differ are returned.
#'
#' @param x (`card`)\cr
#'   first ARD to compare.
#' @param y (`card`)\cr
#'   second ARD to compare.
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns identifying unique records. The intersection of the selected
#'   columns in both ARDs is used. Default is
#'   `c(all_ard_groups(), all_ard_variables(), any_of(c("variable", "variable_level", "stat_name")))`.
#' @param compare ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to compare between the two ARDs. Default is
#'   `any_of(c("stat_label", "stat", "stat_fmt"))`.
#' @param tolerance (`numeric(1)`)\cr
#'   numeric tolerance passed to `all.equal()` for numeric comparisons.
#'   Default is `sqrt(.Machine$double.eps)`.
#' @param check.attributes (`logical(1)`)\cr
#'   logical passed to `all.equal()` indicating whether object attributes
#'   (e.g. names) should be compared. Default is `TRUE`.
#'
#' @return a named list of class `"ard_comparison"` containing:
#'
#'   - `rows_in_x_not_y`: data frame of rows present in `x` but not in `y`
#'     (based on key columns)
#'   - `rows_in_y_not_x`: data frame of rows present in `y` but not in `x`
#'     (based on key columns)
#'   - `compare`: a named list where each element is a data frame containing
#'     the key columns, the compared column values from both ARDs, and a
#'     `difference` column with the `all.equal()` description for rows where
#'     values differ
#'
#' @export
#'
#' @examples
#' base <- ard_summary(ADSL, by = ARM, variables = AGE)
#' compare <- ard_summary(dplyr::mutate(ADSL, AGE = AGE + 1), 
#'                        by = ARM, 
#'                        variables = AGE)
#'
#' compare_ard(base, compare)$compare$stat
#'
compare_ard <- function(x,
                        y,
                        keys = c(all_ard_groups(), all_ard_variables(), any_of(c("variable", "variable_level", "stat_name"))),
                        compare = any_of(c("stat_label", "stat", "stat_fmt")),
                        tolerance = sqrt(.Machine$double.eps),
                        check.attributes = TRUE) {
  set_cli_abort_call()

  check_class(x, cls = "card")
  check_class(y, cls = "card")

  # process keys and compare arguments -----------------------------------------
  keys <- .process_keys_arg(x, y, keys = {{ keys }})
  compare <- .process_compare_arg(x, y, compare = {{ compare }})

  # check for duplicates in keys -----------------------------------------------
  .check_keys_unique(x, keys, arg_name = "x")
  .check_keys_unique(y, keys, arg_name = "y")

  # initialize results list ----------------------------------------------------
  results <- rlang::rep_named(c("rows_in_x_not_y", "rows_in_y_not_x"), list(NULL))
  results[["compare"]] <- rlang::rep_named(compare, list(NULL))

  # find rows present in one ARD but not the other -----------------------------
  results[["rows_in_x_not_y"]] <- .compare_rows(x, y, keys)
  results[["rows_in_y_not_x"]] <- .compare_rows(y, x, keys)

  # compare columns and find mismatches ----------------------------------------
  results[["compare"]] <- .compare_columns(x, y, keys, compare, tolerance, check.attributes)

  # return results with class --------------------------------------------------
  structure(results, class = c("ard_comparison", class(results)))
}
