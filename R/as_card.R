#' Data Frame as ARD
#'
#' Convert data frames to ARDs of class 'card'.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' data.frame(
#'   stat_name = c("N", "mean"),
#'   stat_label = c("N", "Mean"),
#'   stat = c(10, 0.5)
#' ) |>
#'   as_card()
as_card <- function(x) {
  set_cli_abort_call()

  # check in inputs ------------------------------------------------------------
  check_class(x, cls = "data.frame")

  # convert to class "card" ----------------------------------------------------
  if (inherits(x, "card")) {
    x
  } else {
    structure(x, class = c("card", class(x)))
  }
}
