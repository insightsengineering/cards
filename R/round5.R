#' Rounding of Numbers
#'
#' Rounds the values in its first argument to the specified number of
#' decimal places (default 0). Importantly, `round5()` **does not** use Base R's
#' "round to even" default. Standard rounding methods are implemented, for example,
#' `round5(0.5) = 1`.
#'
#' @details
#' Function inspired by `janitor::round_half_up()`.
#'
#' @param x (`numeric`)\cr
#'   a numeric vector
#' @param digits (`integer`)\cr
#'   integer indicating the number of decimal places
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' x <- 0:4 / 2
#' round5(x) |> setNames(x)
#'
#' # compare results to Base R
#' round(x) |> setNames(x)
round5 <- function(x, digits = 0) {
  trunc(abs(x) * 10^digits + 0.5 + sqrt(.Machine$double.eps)) / 10^digits * sign(as.numeric(x))
}
