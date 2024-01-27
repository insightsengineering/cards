#' ARD Selectors
#'
#' @description
#' These selection helpers match variables according to a given pattern.
#'
#' - `all_ard_groups()`: Use this function in dplyr selecting environments, such
#'   as `dplyr::select()`. Function selects grouping columns, e.g. columns
#'   named `"group##"` or `"group##_level"`.
#'
#' - `all_ard_variables()`: Use this function in dplyr selecting environments, such
#'   as `dplyr::select()`. Function selects variables columns, e.g. columns
#'   named `"variable"` or `"variable_level"`.
#'
#' @param variables logical indicating whether to select the columns with the
#' variable names. Default is `TRUE`
#' @param levels logical indicating whether to select the columns with the
#' variable levels. Default is `TRUE`
#' @return tidyselect output
#' @name selectors
#'
#' @examples
#' ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
#'
#' get_ard_statistics(ard, group1_level %in% "Placebo", variable_level %in% "65-80")
NULL

#' @export
#' @rdname selectors
all_ard_groups <- function(variables = TRUE, levels = TRUE) {
  if (isTRUE(variables) && isTRUE(levels)) {
    return(dplyr::matches("^group[0-9]+$|^group[0-9]+_level$"))
  }
  if (isTRUE(variables)) {
    return(dplyr::matches("^group[0-9]+$$"))
  }
  if (isTRUE(levels)) {
    return(dplyr::matches("^group[0-9]+_level$"))
  }
}

#' @export
#' @rdname selectors
all_ard_variables <- function(variables = TRUE, levels = TRUE) {
  if (isTRUE(variables) && isTRUE(levels)) {
    return(dplyr::any_of(c("variable", "variable_level")))
  }
  if (isTRUE(variables)) {
    return(dplyr::any_of("variable"))
  }
  if (isTRUE(levels)) {
    return(dplyr::any_of("variable_level"))
  }
}
