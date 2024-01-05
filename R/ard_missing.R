#' Missing ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for statistics about data missingness.
#'
#' @inheritParams ard_continuous
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   results are tabulated by **all combinations** of the columns specified
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard_missing(ADSL, by = "ARM", variables = "AGE")
ard_missing <- function(data,
                        variables,
                        by = NULL,
                        statistics = everything() ~ missing_variable_summary_fns(),
                        fmt_fn = NULL,
                        stat_labels = everything() ~ default_stat_labels()) {

  # process variable inputs ----------------------------------------------------
  process_selectors(data, variables = {{ variables }})

  # convert all variables to T/F whether it's missing --------------------------
  data <- data |>
    dplyr::mutate(
      across(all_of(variables), Negate(is.na))
    )

  # get the summary statistics -------------------------------------------------
  ard_continuous(
    data = data,
    variables = all_of(variables),
    by = {{ by }},
    statistics = statistics,
    fmt_fn = fmt_fn,
    stat_labels = stat_labels
  ) |>
    dplyr::mutate(
      context = "missing"
    )
}
