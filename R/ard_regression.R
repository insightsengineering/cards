#' Regression ARD
#'
#' Function takes a regression model object and converts it to a ARD
#' structure using the `broom.helpers` package.
#'
#' @param model regression model object
#' @param tidy_fun (`function`)\cr
#'   a tidier. Default is broom.helpers::tidy_with_broom_or_parameters
#' @param ... Arguments passed to `broom.helpers::tidy_plus_plus()`
#'
#' @return data frame
#' @export
#'
#' @examples
#' lm(AGE ~ ARM, data = ADSL) |>
#'   ard_regression(add_estimate_to_reference_rows = TRUE)
ard_regression <- function(model, tidy_fun = NULL, ...) {
  # check installed packages ---------------------------------------------------
  rlang::check_installed("broom.helpers")

  # check inputs ---------------------------------------------------------------
  check_not_missing(model, "model")

  # summarize model ------------------------------------------------------------
  broom.helpers::tidy_plus_plus(
    model = model,
    tidy_fun = tidy_fun %||% broom.helpers::tidy_with_broom_or_parameters,
    ...
  )|>
    dplyr::mutate(
      variable_level = dplyr::if_else(.data$var_type %in% "continuous", NA_character_, .data$label),
      dplyr::across(-c("variable", "variable_level"), .fns = as.list)
    ) |>
    tidyr::pivot_longer(
      cols = -c("variable", "variable_level"),
      names_to = "stat_name",
      values_to = "statistic"
    ) |>
    dplyr::mutate(
      statistic_fmt_fn =
        lapply(
          .data$statistic,
          function(x) switch(is.integer(x), 0L) %||% switch(is.numeric(x), 1L)),
      context = "regression"
    ) |>
    tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))}
}
