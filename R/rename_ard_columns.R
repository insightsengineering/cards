#' Rename ARD Columns
#'
#' This function combines a pair of `group`/`group_level` or `variable`/`variable_level` columns into a
#' single column. The `group_level` or `variable_level` column is renamed according to the value of
#' the `group` or `variable` column, respectively.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#' @param col ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Name of column containing the variable names
#' @param col_lev ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Name of column containing the variable levels. If `NULL` (default), this will be assumed to match the `col` argument with `_level` appended to the end (e.g. if `col` = "group1", `col_lev` will be assumed to be "group1_level")
#'
#' @return data frame
#' @export
#'
#' @examples
#' data <- data.frame(group1 = "A", group1_level = "B", variable = "C", variable_level = "D")
#'
#' cards::rename_ard_columns(data, "group1")
#' cards::rename_ard_columns(data, "variable")
rename_ard_columns <- function(x, col, col_lev = NULL) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(col)

  # process arguments ----------------------------------------------------------
  process_selectors(x,
    col = {{ col }},
    col_lev = {{ col_lev }}
  )
  if (length(col_lev) == 0) {
    col_lev <- paste0(col, "_level")
  }

  col_vals <- unique(x[[col]]) |>
    stats::na.omit() |>
    as.character()
  col_vals_new <- setdiff(col_vals, names(x))

  # rename as the variable level within the unique levels of the grouping variable
  x <- x |>
    # unlist the list-columns & convert NULL to NA
    dplyr::mutate(
      dplyr::across(
        any_of(c(col, col_lev)),
        ~ lapply(., \(x) if (!is.null(x)) as.character(x) else NA_character_) |>
          unlist()
      )
    ) |>
    dplyr::mutate(!!col := fct_inorder(.data[[col]])) |>
    dplyr::group_by(.data[[col]]) |>
    dplyr::group_split() |>
    map(function(dat) {
      col_new <- unique(dat[[col]]) |> as.character()

      # drop if no grouping values
      if (is.na(col_new)) {
        dplyr::select(dat, -any_of(c(col_lev, col)))
      } else {
        # create _level var if it does not exist
        if (is.null(dat[[col_lev]])) {
          dat <- dat |> dplyr::mutate(!!col_lev := NA_character_, .after = all_of(col))
        }

        # fill any NA _level
        col_new_fill <- make.unique(c(
          unique(dat[[col_lev]]) |> unlist(),
          paste("Overall", col_new)
        )) |>
          dplyr::last()

        # rename _level var & drop source
        dat_rnm <- dat %>%
          dplyr::mutate(across(any_of(c(col, col_lev)), as.character)) |>
          dplyr::mutate(!!col_lev := tidyr::replace_na(.data[[col_lev]], col_new_fill))

        if (col_new %in% names(dat_rnm)) {
          # if there are any mismatches between the an existing column and the column-to-be, notify user that column-to-be will take precedence
          if (!all(is.na(dat_rnm[[col_new]])) &&
            !all(is.na(dat_rnm[[col_lev]])) &&
            any(dat_rnm[[col_new]] != dat_rnm[[col_lev]])) {
            cli::cli_alert_warning("Original values of {.val {col_new}} will be overwritten by those from {.val {col_lev}}.")
          }

          dat_rnm <- dat_rnm |>
            dplyr::mutate(!!col_new := ifelse(!is.na(.data[[col_lev]]),
              .data[[col_lev]],
              .data[[col_new]]
            )) |>
            dplyr::relocate(all_of(col_new), .after = all_of(col_lev)) |>
            dplyr::select(-all_of(c(col, col_lev)))
        } else {
          dat_rnm <- dat_rnm |>
            dplyr::rename(!!col_new := all_of(col_lev)) |>
            dplyr::select(-all_of(col))
        }
      }
    })

  x_combined <- dplyr::bind_rows(x)

  # ensure all the newly created appear in sequence
  if (length(col_vals_new) > 1) {
    x_combined <- x_combined |>
      dplyr::relocate(all_of(col_vals_new[-1]), .after = all_of(col_vals_new[1]))
  }

  x_combined
}
