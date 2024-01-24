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


#' Order rows of ARD
#'
#' Order rows of ARD according to variables, groups, and strata, retaining the
#' order of input ARDs
#'
#' @param x (`data.frame`)\cr
#'   a ARD data frame of class 'card'
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard <-
#'   bind_ard(
#'     ard_continuous(mtcars, variables = c("mpg","disp"), by = "cyl"),
#'     ard_ttest(mtcars, variable = "mpg", by = "cyl"),
#'     ard_ttest(mtcars, variable = "disp", by = "cyl")
#'   )
#' tidy_ard_row_order(ard)
tidy_ard_row_order <- function(x){

  # get columns that dictate ordering
  dat <- x |>
    dplyr::select(all_ard_variables(variables = TRUE, levels = FALSE),
                  all_ard_groups(variables = TRUE, levels = FALSE),
                  all_ard_groups(variables = FALSE, levels = TRUE))

  cols <- dat |>
    names()

  # build a tibble of the desired ordering, based on observed data
  dat_order <- dat |>
    dplyr::distinct() |>
    dplyr::mutate(across(everything(),
                         .names = "{.col}_idx",
                         .fns = function(x) match(x, unique(x)))) |>
    dplyr::arrange(across(all_of(paste0(cols, "_idx")))) |>
    dplyr::select(all_of(cols))

  # combine ordering with the original data
  dplyr::left_join(dat_order, x, by = cols) |>
    dplyr::select(all_of(names(x)))
}
