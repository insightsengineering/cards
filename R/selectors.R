#' ARD Selectors
#'
#' @description
#' These selection helpers match variables according to a given pattern.
#'
#' - `all_ard_groups()`: Function selects grouping columns, e.g. columns
#'   named `"group##"` or `"group##_level"`.
#'
#' - `all_ard_variables()`: Function selects variables columns, e.g. columns
#'   named `"variable"` or `"variable_level"`.
#'
#' - `all_ard_group_n()`: Function selects `n` grouping columns.
#'
#' - `all_missing_columns()`: Function selects columns that are all `NA` or empty.
#'
#' @param types (`character`)\cr
#'   type(s) of columns to select. `"names"` selects the columns variable name columns,
#'   and `"levels"` selects the level columns. Default is `c("names", "levels")`.
#' @param n (`integer`)\cr
#'   integer(s) indicating which grouping columns to select.
#'
#' @return tidyselect output
#' @name selectors
#'
#' @examples
#' ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
#'
#' ard |> dplyr::select(all_ard_groups())
#' ard |> dplyr::select(all_ard_variables())
NULL

#' @export
#' @rdname selectors
all_ard_groups <- function(types = c("names", "levels")) {
  types <- arg_match(types, values = c("names", "levels"), multiple = TRUE)

  if (setequal(types, c("names", "levels"))) {
    return(dplyr::matches("^group[0-9]+$|^group[0-9]+_level$"))
  }
  if (setequal(types, "names")) {
    return(dplyr::matches("^group[0-9]+$$"))
  }
  if (setequal(types, "levels")) {
    return(dplyr::matches("^group[0-9]+_level$"))
  }
}

#' @export
#' @rdname selectors
all_ard_variables <- function(types = c("names", "levels")) {
  types <- arg_match(types, values = c("names", "levels"), multiple = TRUE)

  if (setequal(types, c("names", "levels"))) {
    return(dplyr::any_of(c("variable", "variable_level")))
  }
  if (setequal(types, "names")) {
    return(dplyr::any_of("variable"))
  }
  if (setequal(types, "levels")) {
    return(dplyr::any_of("variable_level"))
  }
}

#' @export
#' @rdname selectors
all_ard_group_n <- function(n, types = c("names", "levels")) {
  types <- arg_match(types, values = c("names", "levels"), multiple = TRUE)

  group_cols <- character(0L)
  if ("names" %in% types) group_cols <- c(group_cols, paste0("group", n)) # styler: off
  if ("levels" %in% types) group_cols <- c(group_cols, paste0("group", n, "_level")) # styler: off

  check_integerish(n)
  any_of(sort(group_cols))
}

#' @export
#' @rdname selectors
all_missing_columns <- function() {
  where(\(x) case_switch(is.list(x) ~ all_empty(x), .default = all_na(x)))
}
all_na <- function(x) all(is.na(x))
all_empty <- function(x) all(map_lgl(x, is_empty))
