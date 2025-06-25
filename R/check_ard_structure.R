#' Check ARD Structure
#'
#' Function tests the structure and returns notes when object does not
#' conform to expected structure.
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param column_order (scalar `logical`)\cr
#'   check whether ordering of columns adheres to to `cards::tidy_ard_column_order()`.
#' @param method (scalar `logical`)\cr
#'   check whether a `"stat_name"` equal to `"method"` appears in results.
#' @return an ARD data frame of class 'card' (invisible)
#' @export
#'
#' @examples
#' ard_continuous(ADSL, variables = "AGE") |>
#'   dplyr::select(-warning, -error) |>
#'   check_ard_structure()
check_ard_structure <- function(x, column_order = TRUE, method = TRUE) {
  set_cli_abort_call()
  check_scalar_logical(method)
  check_scalar_logical(column_order)

  # check class ----------------------------------------------------------------
  if (!inherits(x, "card")) {
    cli::cli_inform("Object is not of class {.cls card}.")
  }

  # exit if not a data frame ---------------------------------------------------
  if (!inherits(x, "data.frame")) {
    return(invisible())
  }

  # check expected variables are present ---------------------------------------
  missing_variables <-
    c(
      "variable", "stat_name", "stat_label", "stat",
      "fmt_fun", "warning", "error"
    ) |>
    setdiff(names(x))
  if (!is_empty(missing_variables)) {
    cli::cli_inform("The following columns are not present: {.val {missing_variables}}.")
  }

  # check whether AR contains a method stat ------------------------------------
  if (isTRUE(method)) {
    if (!"method" %in% x$stat_name) {
      cli::cli_inform("Expecting a row with {.code stat_name = 'method'}, but it is not present.")
    }
  }

  # check order of columns -----------------------------------------------------
  if (isTRUE(column_order)) {
    if (!identical(names(x), names(tidy_ard_column_order(x)))) {
      cli::cli_inform(
        c("The column order is not in the standard order.",
          i = "Use {.fun cards::tidy_ard_column_order} for standard ordering."
        )
      )
    }
  }

  # check columns are list columns as expected ---------------------------------
  expected_lst_columns <-
    dplyr::select(
      x, all_ard_groups(), all_ard_variables(),
      any_of(c("stat", "fmt_fun", "warning", "error"))
    ) |>
    # remove group## and variable columns
    dplyr::select(-matches("^group[0-9]$"), -"variable") |>
    names()
  not_a_lst_columns <-
    x[expected_lst_columns] |>
    dplyr::select(-where(is.list)) |>
    names()
  if (!is_empty(not_a_lst_columns)) {
    cli::cli_inform("The following columns are expected to be list columns: {.val {not_a_lst_columns}}.")
  }

  invisible(x)
}
