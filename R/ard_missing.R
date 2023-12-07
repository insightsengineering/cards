#' Missing ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for statistics about data missingness.
#'
#' @inheritParams ard_continuous
#' @param by results are tabulated by **all combinations** of the columns specified
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard_missing(ADSL, by = "ARM", variables = "AGE") |>
#'   flatten_ard()
ard_missing <- function(data,
                        variables,
                        by = NULL,
                        statistics = everything() ~ missing_variable_summary_fns(),
                        fmt_fn = NULL) {
  # process variable inputs ----------------------------------------------------
  process_selectors(data, variables = {{ variables }})

  # convert all variables to T/F whether it's missing --------------------------
  data <- data |>
    dplyr::mutate(
      dplyr::across(all_of(variables), Negate(is.na))
    )

  # get the summary statistics -------------------------------------------------
  ard_continuous(
    data = data,
    variables = all_of(variables),
    by = {{ by }},
    statistics = statistics,
    fmt_fn = fmt_fn
  ) |>
    dplyr::mutate(
      context = "missing"
    )
}
