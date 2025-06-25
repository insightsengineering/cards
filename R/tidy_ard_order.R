#' Standard Order of ARD
#'
#' @description
#' ARD functions for relocating columns and rows to the standard order.
#'
#' - `tidy_ard_column_order()` relocates columns of the ARD to the standard order.
#'
#' - `tidy_ard_row_order()` orders rows of ARD according to groups and
#'   strata (group 1, then group2, etc), while retaining the column order of the input ARD.
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param group_order (`string`)\cr
#'   specifies the ordering of the grouping variables.
#'   Must be one of `c("ascending", "descending")`.
#'   Default is `"ascending"`, where grouping variables begin with `"group1"` variables,
#'   followed by `"group2"` variables, etc.
#'
#' @return an ARD data frame of class 'card'
#' @name tidy_ard_order
#'
#' @examples
#' # order columns
#' ard <-
#'   dplyr::bind_rows(
#'     ard_continuous(mtcars, variables = "mpg"),
#'     ard_continuous(mtcars, variables = "mpg", by = "cyl")
#'   )
#'
#' tidy_ard_column_order(ard) |>
#'   tidy_ard_row_order()
NULL

#' @rdname tidy_ard_order
#' @export
tidy_ard_column_order <- function(x, group_order = c("ascending", "descending")) {
  set_cli_abort_call()
  group_order <- arg_match(group_order)

  # specify the ordering the grouping variables
  group_cols <-
    data.frame(colname = dplyr::select(x, all_ard_groups()) |> names()) |>
    dplyr::arrange(
      case_switch(
        group_order == "ascending" ~ as.integer(unlist(str_extract_all(.data$colname, "\\d+"))),
        group_order == "descending" ~ dplyr::desc(as.integer(unlist(str_extract_all(.data$colname, "\\d+"))))
      ),
      .data$colname
    ) |>
    dplyr::pull("colname")

  # selecting the columns in the tidy order
  dplyr::select(
    x,
    all_of(group_cols),
    all_ard_variables(),
    any_of(c(
      "context",
      "stat_name", "stat_label", "stat", "stat_fmt", "fmt_fun",
      "warning", "error"
    )),
    dplyr::everything()
  )
}


#' @rdname tidy_ard_order
#' @export
tidy_ard_row_order <- function(x) {
  set_cli_abort_call()

  # get columns that dictate ordering
  cols <- x |>
    dplyr::select(all_ard_groups(c("names", "levels"))) |>
    names()
  if (!is_empty(cols)) {
    max_group_n <- as.integer(unlist(str_extract_all(cols, "\\d+"))) |> max()
    cols <-
      map(seq_len(max_group_n), ~ c(paste0("group", .x), paste0("group", .x, "_level"))) |>
      unlist() |>
      intersect(cols)
  }

  # perform the ordering
  x |> dplyr::arrange(across(all_of(cols), .fns = function(x) match(x, unique(x))))
}
