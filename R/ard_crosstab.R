#' ARD Crosstab
#'
#' Similar to `ard_categorical()` except the percentages and N statistics are
#' based on the entire data set, rather than stratified columns.
#'
#' @inheritParams ard_categorical
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to compute statistics by.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard_crosstab(ADSL, variables = "AGEGR1", by = "ARM")
ard_crosstab <- function(data, variables, by) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variables, "variables")
  check_not_missing(by, "by")
  check_class_data_frame(data = data)

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  process_selectors(data, variables = {{ variables }}, by = {{ by }})

  # return empty tibble if no variables selected -------------------------------
  if (rlang::is_empty(variables)) return(dplyr::tibble())

  # tabulate crosstab ----------------------------------------------------------
  lapply(
    variables,
    FUN = function(variable) {
      denominator <-
        data |>
        nest_for_ard(by = by, rename_columns = FALSE) |>
        dplyr::mutate(
          data = .env$data[c(by, variable)] |> tidyr::drop_na() |> dplyr::select(all_of(variable)) |> list()
        ) |>
        tidyr::unnest(cols = dplyr::everything())

      ard_categorical(data = data, variables = all_of(variable), by = all_of(by), denominator = denominator)
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(context = "crosstab")
}
