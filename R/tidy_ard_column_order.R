#' Standard Column Order of ARD
#'
#' Relocate all columns in ARD to the standard order.
#'
#' @param x (`data.frame`)\cr
#'   a ARD data frame of class 'card'
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard <-
#'   dplyr::bind_rows(
#'     ard_continuous(mtcars, variables = "mpg"),
#'     ard_continuous(mtcars, variables = "mpg", by = "cyl")
#'   )
#' names(ard)
#'
#' tidy_ard_column_order(ard)
tidy_ard_column_order <- function(x) {
  group_cols <- dplyr::select(x, all_ard_groups()) |> names() |> sort()

  dplyr::select(
    x,
    all_of(group_cols),
    all_ard_variables(),
    any_of(c(
      "context",
      "stat_name", "stat_label", "statistic", "statistic_fmt", "statistic_fmt_fn",
      "warning", "error"
    )),
    dplyr::everything()
  )
}
