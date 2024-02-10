#' ARD Selectors
#'
#' @description
#' These selection helpers match variables according to a given pattern.
#'
#' - `all_ard_groups()`: Use this function in dplyr selecting environments, such
#'   as [dplyr::select()]. Function selects grouping columns, e.g. columns
#'   named `"group##"` or `"group##_level"`.
#'
#' - `all_ard_variables()`: Use this function in dplyr selecting environments, such
#'   as `dplyr::select()`. Function selects variables columns, e.g. columns
#'   named `"variable"` or `"variable_level"`.
#'
#' @param types (`character`)\cr
#'   type(s) of columns to select. `"names"` selects the columns variable name columns,
#'   and `"levels"` selects the level columns. Default is `c("names", "levels")`.
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
