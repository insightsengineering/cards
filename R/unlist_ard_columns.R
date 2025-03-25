#' Unlist ARD Columns
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card' or any data frame
#' @param columns ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to unlist. Default is
#'   `c(where(is.list), -any_of(c("warning", "error", "fmt_fn")))`.
#' @param fill (scalar)\cr
#'   scalar to fill NULL values with before unlisting (if they are present).
#'   Default is `NA`.
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' ADSL |>
#'   ard_categorical(by = ARM, variables = AGEGR1) |>
#'   apply_fmt_fn() |>
#'   unlist_ard_columns()
#'
#' ADSL |>
#'   ard_continuous(by = ARM, variables = AGE) |>
#'   apply_fmt_fn() |>
#'   unlist_ard_columns()
unlist_ard_columns <- function(x,
                               columns = c(where(is.list), -any_of(c("warning", "error", "fmt_fn"))),
                               fill = NA) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_data_frame(x)
  process_selectors(x, columns = {{ columns }})
  check_scalar(fill)

  # first replace any NULL values with the fill value --------------------------
  x <- x |>
    dplyr::mutate(
      across(all_of(columns), ~map(., \(value) value %||% .env$fill))
    )

  # unlist the columns ---------------------------------------------------------
  for (var in columns) {
    var_unlisted <- unlist(x[[var]])
    if (length(var_unlisted) != length(x[[var]])) {
      cli::cli_inform("Cannot unlist column {.val {var}}.")
      next
    }
    x[[var]] <- var_unlisted
  }

  # return unlisted object -----------------------------------------------------
  x
}
