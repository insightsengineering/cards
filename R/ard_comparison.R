#' Comparison ARD Statistics
#'
#' @param data a data frame
#' @param by charcter column name to compare by
#' @param variable charadter column name to be compared
#' @param ... arguments passed to method.
#'
#' @return data frame
#' @name ard_comparison
#'
#' @examples
#' ard_ttest(data = mtcars, by = "am", variable = "hp")
NULL

#' @rdname ard_comparison
#' @export
ard_ttest <- function(data, by, variable, conf.level = 0.95, ...) {
  # check installed packages ---------------------------------------------------
  rlang::check_installed("broom")

  # perform t-test and format results ------------------------------------------
  stats::t.test(data[[variable]] ~ data[[by]], conf.level = conf.level, ...) |>
    broom::tidy() |>
    dplyr::mutate(
      conf.level = conf.level,
      dplyr::across(everything(), .fns = list),
      strata1 = by,
      variable = variable,
      context = "ttest"
    ) |>
    tidyr::pivot_longer(
      cols = -c("strata1", "variable", "context"),
      names_to = "stat_name",
      values_to = "statistic"
    ) |>
    dplyr::mutate(
      strata1_level =
        dplyr::case_when(
          .data$stat_name %in% "estimate1" ~ unique(data[[by]]) |> stats::na.omit() |>  sort() |> dplyr::first() |> list(),
          .data$stat_name %in% "estimate2" ~ unique(data[[by]]) |> stats::na.omit() |>sort() |> dplyr::last() |> list(),
        )
    )
}
