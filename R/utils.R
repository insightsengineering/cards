#' Convert Data Frame to Named List
#'
#' Function takes a two column data frame as inputs.
#' The first column will be the names of the resulting list, and the second
#' column the values of the list.
#'
#' @param x a data frame
#'
#' @return a named list
#' @noRd
.data_frame_to_named_list <- function(x) {
  as.list(x[[2]]) |> stats::setNames(nm = x[[1]])
}

#' Get Variable Label from ARD
#'
#' Extract a variable's label from the ARD's 'label' statistic.
#' If it is not found, the variable name is returned as default.
#'
#' @param ard an ARD
#' @param variable a variable name string
#'
#' @return a string
#' @noRd
.get_ard_label_statistic <- function(ard, variable = NULL) {
  # subset by variable name if supplied
  if (!is.null(variable)) {
    ard <- dplyr::filter(ard, .data$variable %in% .env$variable)
  }

  # filter on the label statistic
  ard_subset <- dplyr::filter(ard, .data$stat_name %in% 'label')

  # select the label and return. If empty, NULL is returned
  unlist(ard_subset$statistic) %||%
    ard$variable[1]
}


#' ARD-flavored Nesting
#'
#' This function is similar to `tidyr::nest()`, except that it retains
#' rows for unobserved factor levels, and unobserved combinations of
#' stratifying variables.
#'
#' The levels are wrapped in lists so they can be stacked with other types
#' of different clases
#'
#' @param data a data frame
#' @param by a character string of stratifying variable names
#' @param key the name of the new column with the nested data frame.
#' Default is `"data"`
#'
#' @return a nested data frame
#' @noRd
.ard_nest <- function(data, by = NULL, key = "data") {
  # if no stratifying variables, simply return the data frame
  if (rlang::is_empty(by)) return((dplyr::tibble("{key}" := list(data))))

  n_missing <- nrow(data) - nrow(tidyr::drop_na(data, all_of(by)))
  if (n_missing > 0L) {
    cli::cli_inform("{n_missing} missing observation{?s} in the {.val {by}} column{?s} have been removed.")
  }

  # get a named list of all unique values for each by variable (including unobserved levels)
  lst_unique_vals <-
    by |>
    lapply(FUN = function(x) data[[x]] |> .unique_and_sorted()) |>
    stats::setNames(nm = by)

  # convert that list to a data frame with one row per unique combination
  df_return <- tidyr::expand_grid(!!!lst_unique_vals)

  # we will now add a column to the df_return data frame of the subsetted data
  #   to do so, we'll construct a list of expressions that can be passed to
  #   dplyr::filter() to subset the data frame
  lst_filter_exprs <-
    seq_len(nrow(df_return)) |>
    lapply(
      FUN = function(i) {
        lapply(
          X = by,
          FUN = function(z) {
            rlang::expr(!!rlang::data_sym(z) %in% df_return[[!!z]][!!i])
          }
        )
      }
    )

  # now adding the subsetted data frames to the nested tibble
  df_return[[key]] <-
    lapply(
      seq_len(nrow(df_return)),
      FUN = function(i) {
        dplyr::filter(data, !!!lst_filter_exprs[[i]]) |>
          dplyr::select(-all_of(.env$by))
      }
    )

  # returning final nested tibble
  df_return |>
    dplyr::mutate(dplyr::across(.cols = -dplyr::all_of(key), .fns = as.list))
}

#' ARD-flavor of unique()
#'
#' Essentially a wrapper for `unique(x) |> sort()` with NA levels removed.
#' Expect in the case of factor and logical vectors.
#' For factors, all levels are returned even if they are unobserved.
#' Similarly, logical vectors always return `c(TRUE, FALSE)`, even if
#' both levels are not observed.
#'
#' @param x a vector
#' @return a vector
#' @noRd
.unique_and_sorted <- function(x) {
  # if a factor return a factor that includes the same levels (including unobserved levels)
  if (inherits(x, "factor")) {
    return(factor(levels(x), levels = levels(x)))
  }
  if (inherits(x, "logical")) {
    return(c(TRUE, FALSE))
  }

  # otherwise, return a simple unique and sort of the vector
  unique(x) |> sort()
}


#' Named list predicate
#'
#' A predicate function whether input is a named list and _not_ a data frame.
#'
#' @param x object to check
#' @keywords internal
#' @noRd
.is_named_list <- function(x, allow_df = FALSE) {
  if (isFALSE(allow_df))
    return(is.list(x) && rlang::is_named(x) && !is.data.frame(x))
  if (isTRUE(allow_df))
    return(is.list(x) && rlang::is_named(x))
}

#' A list_flatten()-like function
#'
#' Function operates similarly to `purrr::list_flatten(x, name_spec = "{inner}")`
#'
#' @param x a named list
#' @keywords internal
#' @noRd
.purrr_list_flatten <- function(x) {
  ret <- list()

  for (i in seq_along(x)) {
    if (.is_named_list(x[[i]])) ret <- append(ret, values = x[[i]])
    else ret <- append(ret, values = x[i])
  }

  ret
}

