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
  check_pkg_installed("broom.helpers", pkg_search = "cards")

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
      across(-c("variable", "variable_level"), .fns = as.list)
    ) |>
    tidyr::pivot_longer(
      cols = -c("variable", "variable_level"),
      names_to = "stat_name",
      values_to = "statistic"
    ) |>
    dplyr::filter(map_lgl(.data$statistic, Negate(is.na))) |>
    dplyr::mutate(
      statistic_fmt_fn =
        lapply(
          .data$statistic,
          function(x) switch(is.integer(x), 0L) %||% switch(is.numeric(x), 1L)),
      context = "regression",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "var_label" ~ "Label",
          .data$stat_name %in% "var_class" ~ "Class",
          .data$stat_name %in% "var_type" ~ "Type",
          .data$stat_name %in% "var_nlevels" ~ "N Levels",
          .data$stat_name %in% "contrasts_type" ~ "Contrast Type",
          .data$stat_name %in% "label" ~ "Level Label",
          .data$stat_name %in% "n_obs" ~ "N Obs.",
          .data$stat_name %in% "n_event" ~ "N Events",
          .data$stat_name %in% "exposure" ~ "Exposure Time",
          .data$stat_name %in% "estimate" ~ "Coefficient",
          .data$stat_name %in% "std.error" ~ "Standard Error",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "conf.low" ~ "CI Lower Bound",
          .data$stat_name %in% "conf.high" ~ "CI Upper Bound",
          TRUE ~ .data$stat_name
        )
    ) |>
    tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))}
}
