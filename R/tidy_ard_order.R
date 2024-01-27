#' Standard Order of ARD
#'
#' @description
#' ARD functions for relocating columns and rows to the standard order
#'
#' - `tidy_ard_column_order()` relocates columns of the ARD to the standard order
#'
#' - `tidy_ard_row_order()` orders rows of ARD according to variables, groups, and strata, while retaining the order of the input ARD
#'
#' @param x (`data.frame`)\cr
#'   a ARD data frame of class 'card'
#'
#' @return a data frame
#' @name tidy_ard_order
#'
#' @examples
#'
#' # order columns
#' ard <-
#'   dplyr::bind_rows(
#'     ard_continuous(mtcars, variables = "mpg"),
#'     ard_continuous(mtcars, variables = "mpg", by = "cyl")
#'   )
#' names(ard)
#'
#' tidy_ard_column_order(ard)
#'
#' # order rows
#' dplyr::bind_rows(
#'   ard_continuous(mtcars, variables = c("mpg", "disp"), by = "cyl"),
#'   ard_ttest(mtcars, variable = "mpg", by = "cyl"),
#'   ard_ttest(mtcars, variable = "disp", by = "cyl")
#' ) |>
#'   tidy_ard_row_order()
#'
#' NULL
#'
#' @rdname tidy_ard_order
#' @export
tidy_ard_column_order <- function(x) {
  group_cols <- dplyr::select(x, all_ard_groups()) |>
    names() |>
    sort()

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


#' @rdname tidy_ard_order
#' @export
tidy_ard_row_order <- function(x) {
  check_class(x, class = "card")

  # get columns that dictate ordering
  dat <- x |>
    dplyr::select(
      all_ard_variables(variables = TRUE, levels = FALSE),
      all_ard_groups(variables = TRUE, levels = FALSE),
      all_ard_groups(variables = FALSE, levels = TRUE)
    )

  cols <- dat |>
    names()

  # perform the ordering
  x |> dplyr::arrange(across(all_of(cols), .fns = function(x) match(x, unique(x))))
}
