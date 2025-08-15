#' Maximum Value
#'
#' For each column in the passed data frame, the function returns a named list
#' with the value being the largest/last element after a sort.
#' For factors, the last level is returned, and for logical vectors `TRUE` is returned.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#'
#' @return a named list
#' @export
#'
#' @examples
#' ADSL[c("AGEGR1", "BMIBLGR1")] |> maximum_variable_value()
maximum_variable_value <- function(data) {
  data |>
    lapply(
      function(x) {
        if (inherits(x, "factor")) {
          return(levels(x) |> dplyr::last())
        }
        if (inherits(x, "logical")) {
          return(TRUE)
        }
        stats::na.omit(x) |>
          unique() |>
          sort() |>
          dplyr::last()
      }
    )
}
