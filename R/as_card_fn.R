#' As card function
#'
#' Add attributes to a function that specify the expected results.
#' It is used when `ard_continuous()` or `ard_complex()` errors and constructs
#' an ARD with the correct structure when the results cannot be calculated.
#'
#' @param f (`function`)\cr
#'   a function
#' @param stat_names (`character`)\cr
#'   a character vector of the expected statistic names returned by function `f`
#'
#' @return an ARD data frame of class 'card'
#' @name as_cards_fn
#'
#' @examples
#' # When there is no error, everything works as if we hadn't used `as_card_fn()`
#' ttest_works <-
#'   as_cards_fn(
#'     \(x) t.test(x)[c("statistic", "p.value")],
#'     stat_names = c("statistic", "p.value")
#'   )
#' ard_continuous(
#'   mtcars,
#'   variables = mpg,
#'   statistic = ~ list(ttest = ttest_works)
#' )
#'
#' # When there is an error and we use `as_card_fn()`,
#' #   we will see the same structure as when there is no error
#' ttest_error <-
#'   as_cards_fn(
#'     \(x) {
#'       t.test(x)[c("statistic", "p.value")]
#'       stop("Intentional Error")
#'     },
#'     stat_names = c("statistic", "p.value")
#'   )
#' ard_continuous(
#'   mtcars,
#'   variables = mpg,
#'   statistic = ~ list(ttest = ttest_error)
#' )
#'
#' # if we don't use `as_card_fn()` and there is an error,
#' #   the returned result is only one row
#' ard_continuous(
#'   mtcars,
#'   variables = mpg,
#'   statistic = ~ list(ttest = \(x) {
#'     t.test(x)[c("statistic", "p.value")]
#'     stop("Intentional Error")
#'   })
#' )
NULL

#' @rdname as_cards_fn
#' @export
as_cards_fn <- function(f, stat_names) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(f, "function")
  check_class(stat_names, "character")

  # add attribute --------------------------------------------------------------
  attr(f, "stat_names") <- stat_names

  # return function and add a class --------------------------------------------
  structure(f, class = c("cards_fn", class(f)))
}

#' @rdname as_cards_fn
#' @export
is_cards_fn <- function(f) {
  inherits(f, "cards_fn")
}

#' @rdname as_cards_fn
#' @export
get_cards_fn_stat_names <- function(f) {
  check_class(f, "cards_fn")
  attr(f, "stat_names")
}
