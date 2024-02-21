#' Check ARD Structure
#'
#' Function tests the structure and returns notes when object does not
#' conform to expected structure.
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#'
#' @return an ARD data frame of class 'card' (invisible)
#' @export
#'
#' @examples
#' ard_continuous(ADSL, variables = "AGE") |>
#'   dplyr::select(-warning, -error) |>
#'   check_ard_structure()
check_ard_structure <- function(x) {
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
      "fmt_fn", "warning", "error"
    ) |>
    setdiff(names(x))
  if (!is_empty(missing_variables)) {
    cli::cli_inform("The following columns are not present: {.val {missing_variables}}.")
  }

  # check columns are list columns as expected ---------------------------------
  expected_lst_columns <-
    dplyr::select(
      x, all_ard_groups(), all_ard_variables(),
      any_of(c("stat", "fmt_fn", "warning", "error"))
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
