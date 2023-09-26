#' ARD Crosstab
#'
#' Similar to `ard_categorical()` except the percentages and N statistics are
#' based on the entire data set, rather than stratified by the columns
#' specified in the `by` argument.
#'
#' @inheritParams ard_categorical
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard_crosstab(ADSL, variables = "AGEGR1", by = "ARM")
ard_crosstab <- function(data, variables, by) {
  # process arguments -----------------------------------------------------------
  by <- dplyr::select(data, {{ by }}) |> colnames()
  variables <- dplyr::select(data, {{ variables }}) |> colnames() |> setdiff(by)
  data <- dplyr::ungroup(data)

  # tabulate crosstab ----------------------------------------------------------
  lapply(
    variables,
    FUN = function(variable) {
      denominator <-
        data |>
        .ard_nest(by = by) |>
        dplyr::mutate(
          data = .env$data[c(by, variable)] |> tidyr::drop_na() |> dplyr::select(all_of(variable)) |> list()
        ) |>
        tidyr::unnest(cols = dplyr::everything())

      ard_categorical(data = data, variables = variable, by = by, denominator = denominator)
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(context = "crosstab")
}
