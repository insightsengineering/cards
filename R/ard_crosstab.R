#' Cross Tabulation ARD
#'
#' Produces summary statistics similar to those that would appear in a
#' cross tabulation of two variables.
#' The rows of the cross tabulation are the columns named in the
#' `variables` argument, and the column of the cross tabulation is the column
#' named in the `by` argument.
#'
#' @inheritParams ard_categorical
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   a single column to compute statistics by.
#' @param percent (`string`)\cr
#'   one of `c("cell", "column", "row")` that determines how the percentage is
#'   calculated. Default is `"cell"`
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard_crosstab(ADSL, variables = "AGEGR1", by = "ARM")
ard_crosstab <- function(data, variables, by,
                         percent = c("cell", "column", "row"),
                         statistics = everything() ~ categorical_variable_summary_fns(),
                         fmt_fn = everything() ~ default_fmt_fns(),
                         stat_labels = everything() ~ default_stat_labels()) {
  # process arguments ----------------------------------------------------------
  percent <- rlang::arg_match(percent)
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(by)
  check_class_data_frame(data = data)
  process_selectors(data, variables = {{ variables }}, by = {{ by }})
  check_length(by, length = 1L)


  # return empty tibble if no variables selected -------------------------------
  if (rlang::is_empty(variables)) return(dplyr::tibble())

  # calculate summary stats ----------------------------------------------------
  switch(
    percent,
    "column" =
      .ard_crosstab_column(data = data, variables = variables,
                        by = by, statistics = statistics,
                        fmt_fn = fmt_fn, stat_labels = stat_labels),
    "row" =
      .ard_crosstab_row(data = data, variables = variables,
                        by = by, statistics = statistics,
                        fmt_fn = fmt_fn, stat_labels = stat_labels),
    "cell" =
      .ard_crosstab_cell(data = data, variables = variables,
                         by = by, statistics = statistics,
                         fmt_fn = fmt_fn, stat_labels = stat_labels)
  ) |>
    dplyr::mutate(context = "crosstab")
}

.ard_crosstab_column <- function(data, variables, by, statistics, fmt_fn, stat_labels) {
  ard_categorical(data, variables = all_of(variables), by = all_of(by),
                  statistics = statistics, fmt_fn = fmt_fn, stat_labels = stat_labels)
}

.ard_crosstab_row <- function(data, variables, by, statistics, fmt_fn, stat_labels) {
  process_formula_selectors(
    data = data[variables],
    statistics = statistics,
    stat_labels = stat_labels,
    fmt_fn = fmt_fn
  )

  # tabulate crosstab ----------------------------------------------------------
  lapply(
    variables,
    FUN = function(variable) {
      if (!rlang::is_empty(fmt_fn[[variable]])) {
        fmt_fn <- fmt_fn[variable] |> stats::setNames(by)
      }
      ard_categorical(data, variables = all_of(by), by = all_of(variable),
                      statistics = statistics[variable] |> stats::setNames(by),
                      fmt_fn = fmt_fn,
                      stat_labels = stat_labels[variable] |> stats::setNames(by)) %>%
        # filling in N stat across rows
        {dplyr::bind_rows(
          dplyr::filter(., !.data$stat_name %in% "N"),
          dplyr::filter(., .data$stat_name %in% "N") |>
            dplyr::mutate(
              variable_level = list(.$variable_level |> unique() |> compact())
            ) |>
            tidyr::unnest("variable_level")
        )}
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::rename(
      group1 = "variable", group1_level = "variable_level",
      variable = "group1", variable_level = "group1_level"
    ) |>
    cards::tidy_ard_column_order()
}

.ard_crosstab_cell <- function(data, variables, by, statistics, fmt_fn, stat_labels) {
  # tabulate crosstab ----------------------------------------------------------
  lapply(
    variables,
    FUN = function(variable) {
      denominator <-
        data[by] |>
        dplyr::distinct() |>
        dplyr::mutate(
          ...ard_denom_n... =
            data |> tidyr::drop_na(all_of(c(by, variable))) |> nrow()
        ) |>
        tidyr::uncount(weights = .data$...ard_denom_n...)

      ard_categorical(data,
                      variables = all_of(variable), by = all_of(by),
                      denominator = denominator,
                      statistics = statistics,
                      fmt_fn = fmt_fn,
                      stat_labels = stat_labels)
    }
  ) |>
    dplyr::bind_rows()
}
