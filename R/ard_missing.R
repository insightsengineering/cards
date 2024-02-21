#' Missing ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for statistics related to data missingness.
#'
#' @inheritParams ard_continuous
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   results are tabulated by **all combinations** of the columns specified.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_missing(ADSL, by = "ARM", variables = "AGE")
#'
#' ADSL |>
#'   dplyr::group_by(ARM) |>
#'   ard_missing(
#'     variables = "AGE",
#'     statistic = ~ missing_summary_fns("N_miss")
#'   )
ard_missing <- function(data,
                        variables,
                        by = dplyr::group_vars(data),
                        statistic = everything() ~ missing_summary_fns(),
                        fmt_fn = NULL,
                        stat_label = everything() ~ default_stat_labels()) {
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
    statistic = statistic,
    fmt_fn = fmt_fn,
    stat_label = stat_label
  ) |>
    dplyr::mutate(
      context = "missing"
    )
}
