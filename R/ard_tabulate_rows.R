#' Row Tabulate ARD
#'
#' Tabulate the number of rows in a data frame.
#'
#' @inheritParams ard_tabulate_value
#' @param colname (`string`)\cr
#'   name of the column that will be returned along with the row tabulation.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_tabulate_rows(ADSL, by = TRTA)
ard_tabulate_rows <- function(data,
                              colname = "..row_count..",
                              by = dplyr::group_vars(data),
                              strata = NULL,
                              statistic = everything() ~ "n",
                              denominator = "column",
                              fmt_fun = NULL,
                              stat_label = everything() ~ default_stat_labels()) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_data_frame(data)
  check_string(colname)

  # tabulate number of rows ----------------------------------------------------
  ard_tabulate_value(
    data = dplyr::mutate(data, "{colname}" := TRUE),
    variables = all_of(colname),
    by = {{ by }},
    strata = {{ strata }},
    statistic = statistic,
    denominator = denominator,
    fmt_fun = fmt_fun,
    stat_label = stat_label,
    value = list(TRUE) |> setNames(colname)
  )
}
