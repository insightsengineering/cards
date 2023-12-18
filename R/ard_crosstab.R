#' ARD Crosstab
#'
#' Produces summary statistics similar to those that would appear in a
#' cross tabulation of two variables.
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
                         statistics = everything() ~ categorical_variable_summary_fns(),
                         percent = c("cell", "column", "row")) {
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
                        by = by, statistics = statistics),
    "row" =
      .ard_crosstab_row(data = data, variables = variables,
                        by = by, statistics = statistics),
    "cell" =
      .ard_crosstab_cell(data = data, variables = variables,
                         by = by, statistics = statistics)
  ) |>
    dplyr::mutate(context = "crosstab")
}

.ard_crosstab_column <- function(data, variables, by, statistics) {
  ard_categorical(data, variables = all_of(variables), by = all_of(by))
}

.ard_crosstab_row <- function(data, variables, by, statistics) {
  # tabulate crosstab ----------------------------------------------------------
  lapply(
    variables,
    FUN = function(variable) {
      ard_categorical(data, variables = all_of(by), by = all_of(variable)) %>%
        # filling in N stat across rows
        {dplyr::bind_rows(
          dplyr::filter(., !.data$stat_name %in% "N"),
          dplyr::filter(., .data$stat_name %in% "N") |>
            dplyr::mutate(
              variable_level = list(.$variable_level |> unique() |> compact())
            ) |>
            tidyr::unnest(.data$variable_level)
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

.ard_crosstab_cell <- function(data, variables, by, statistics) {
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

      ard_categorical(data, variables = all_of(variable), by = all_of(by), denominator = denominator)
    }
  ) |>
    dplyr::bind_rows()
}
