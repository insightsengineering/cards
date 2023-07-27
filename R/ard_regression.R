#' Regression ARD
#'
#' Function takes a regression model object and converts it to a ARD
#' structure using the `broom.helpers` package.
#'
#' @param model regression model object
#' @param tidy_fun a tidier. Default is broom.helpers::tidy_with_broom_or_parameters
#' @param ... Arguments passed to `broom.helpers::tidy_plus_plus()`
#'
#' @return data frame
#' @export
#'
#' @examples
#' glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial) |>
#'   ard_regression(add_estimate_to_reference_rows = TRUE)
ard_regression <- function(model, tidy_fun = NULL, ...) {
  rlang::check_installed("broom.helpers")

  broom.helpers::tidy_plus_plus(model = model, tidy_fun = tidy_fun %||% broom.helpers::tidy_with_broom_or_parameters, ...) |>
    dplyr::mutate(
      variable_level = dplyr::if_else(.data$var_type %in% "continuous", NA_character_, .data$label),
      dplyr::across(-c("variable", "variable_level"), .fns = as.list)
    ) |>
    tidyr::pivot_longer(
      cols = -c("variable", "variable_level"),
      names_to = "stat_name",
      values_to = "statistic"
    )
}
