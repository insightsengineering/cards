#' Replace NULL Statistics with Specified Value
#'
#' When a statistical summary function errors, the `"statistic"` column will be
#' `NULL`. It is, however, sometimes useful to replace these values with a
#' non-`NULL` value, e.g. `NA`.
#'
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param value (usually a `scalar`)\cr
#'   The value to replace `NULL` values with. Default is `NA`.
#' @param rows ([`data-masking`][rlang::args_data_masking])\cr
#'   Expression that return a logical value, and are defined in terms of the variables in `.data`.
#'   Only rows for which the condition evaluates to `TRUE` are replaced.
#'   Default is `TRUE`, which applies to all rows.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' # the quantile functions error because the input is character, while the median function returns NA
#' data.frame(x = rep_len(NA_character_, 10)) |>
#'   ard_continuous(
#'     variables = x,
#'     statistic = ~ continuous_summary_fns(c("median", "p25", "p75"))
#'   ) |>
#'   replace_null_statistic(rows = !is.null(error))
replace_null_statistic <- function(x, value = NA, rows = TRUE) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(x, "card")

  # replace NULL values --------------------------------------------------------
  x |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # styler: off
      stat =
        if (is.null(.data$stat) && {{ rows }}) list(.env$value)
        else list(.data$stat)
      # styler: on
    ) |>
    # restore previous grouping structure and original class of x
    dplyr::group_by(dplyr::pick(dplyr::group_vars(x))) |>
    structure(class = class(x))
}
