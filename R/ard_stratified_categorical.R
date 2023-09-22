#' Stratified Categorical ARD Statistics
#'
#' Similar to `ard_categorical()` except each call to `ard_categorical()`
#' is called within each stratum level.
#' The stratum variable and levels are returned in the ARD in columns
#' `"group##"` and `"group##_level"`
#'
#' @param strata a single column from `data`.
#' @inheritParams ard_categorical
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard_stratified_categorical(
#'   data = ADAE |> dplyr::filter(AOCCPFL %in% "Y"),
#'   strata = "AESOC",
#'   variables = "AEDECOD",
#'   denominator = ADSL
#' )
ard_stratified_categorical <- function(data, strata, variables, by = NULL, denominator = NULL) {
  # process arguments ----------------------------------------------------------
  .process_args_data_variable_by(data, variables, by)
  strata <- dplyr::select(data, {{ strata }}) |> colnames()
  if (!rlang::is_string(strata)) {
    cli::cli_abort("The {.arg strata} argument must select one and only only one column.")
  }

  # convert strata to character if factor to only report on observed stratum
  data <- dplyr::mutate(data, dplyr::across(all_of(strata), ~ifelse(is.factor(.), as.character(.), .)))

  # cycle over strata levels to perform tabulations
  data |>
    .ard_nest(by = strata) |>
    dplyr::rename("group{length(by)+1L}_level" := all_of(strata)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      "group{length(by)+1L}" := .env$strata,
      ard =
        ard_categorical(
          data = .data$data,
          variables = {{ variables }},
          by = {{ by }},
          denominator = denominator
        ) |>
        list()
    ) |>
    dplyr::select(-"data") |>
    tidyr::unnest(cols = -dplyr::ends_with("_level")) |>
    dplyr::mutate(context = "ard_stratified_categorical") |>
    tidy_ard_column_order()  %>%
    structure(., class = c("card", class(.)))
}

