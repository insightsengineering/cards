#' Rename ARD Columns
#'
#' This function combines a pair of `group`/`group_level` or `variable`/`variable_level` columns into a
#' single column. The `group_level` or `variable_level` column is renamed according to the value of
#' the `group` or `variable` column, respectively.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#' @param columns ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Name of columns to coalesce together and rename.
#'
#' @return data frame
#' @export
#'
#' @examples
#' data <- data.frame(group1 = "A", group1_level = "B", variable = "C", variable_level = "D")
#'
#' rename_ard_columns(data)
#' rename_ard_columns(data, columns = c("group1", "group1_level"))
rename_ard_columns <- function(x, columns = c(all_ard_groups(), all_ard_variables())) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(col)

  # process arguments ----------------------------------------------------------
  process_selectors(x, columns = {{ columns }})

  if (length(columns) == 0) {
    return(x)
  }

  # determine pairs of variables and levels ------------------------------------
  column_pairs <- .pair_columns(x, columns)

  # Sequentially coalesce/rename -----------------------------------------------
  for (col_pair in column_pairs) {
    x <- .shift_column_pair(x, col_pair)
  }

  x
}


#' Pair columns
#'
#' This function ingests an ARD object and finds pairs of columns based on those requested for coalescing/renaming
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#' @param columns (`character`)\cr
#'   all columns to consider for coalescing/renaming
#'
#' @return a list of column pairs (as character vectors)
#' @keywords internal
#'
#' @examples
#' ard_categorical(ADSL, by = "ARM", variables = "AGEGR1") |>
#'   cards:::.pair_columns(columns = c("group1", "group1_level", "variable", "variable_level"))
.pair_columns <- function(x, columns) {
  # if `x` is the result of `shuffle_ard` then only columns to be coalesced/renamed will be variable/label
  if (identical(sort(columns), c("label", "variable"))) {
    list(c("variable", "label"))
  } else {
    col_vars <- columns[!grepl(".*_level$", columns)]

    # determine if any of the columns of variables do not have a matching column of levels
    col_levs <- columns[grepl(".*_level$", columns)]
    unmatched_lev <- setdiff(col_levs, paste0(col_vars, "_level"))
    if (length(unmatched_lev) > 0) {
      cli::cli_alert_warning("The following `*_level` columns do not have a match and will not be renamed: {.val {unmatched_lev}}")
    }

    # return a pair of columns (ok if the _level doesn't actually exist)
    lapply(col_vars, function(col) {
      col_lev <- paste0(col, "_level")
      c(col, col_lev)
    })
  }
}

#' Shift column pair
#'
#' This function ingests an ARD object and coalesces/renames a given pair of columns (variable and levels)
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#' @param col_pair (`character`)\cr
#'   character vector containing the column names for variables (first element) and their corresponding levels (second element)
#'
#' @return a tibble
#' @keywords internal
#'
#' @examples
#' ard_categorical(ADSL, by = "ARM", variables = "AGEGR1") |>
#'   cards:::.shift_column_pair(col_pair = c("group1", "group1_level"))
#'
.shift_column_pair <- function(x, col_pair) {
  col <- col_pair[1]
  col_lev <- col_pair[2]

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
