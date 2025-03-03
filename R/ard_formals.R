#' Argument Values ARD
#'
#' Place default and passed argument values to a function into an ARD structure.
#'
#' @param fun (`function`)\cr
#'   a [function] passed to `formals(fun)`
#' @param arg_names (`character`)\cr
#'   character vector of argument names to return
#' @param passed_args (named `list`)\cr
#'   a named list of user-passed arguments. Default is `list()`, which returns
#'   all default values from a function
#' @param envir (`environment`)\cr
#'   an environment passed to `formals(envir)`
#'
#' @return an partial ARD data frame of class 'card'
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' # add the `mcnemar.test(correct)` argument to an ARD structure
#' ard_formals(fun = mcnemar.test, arg_names = "correct")
#'
#' # Example 2 ----------------------------------
#' # S3 Methods need special handling to access the underlying method
#' ard_formals(
#'   fun = asNamespace("stats")[["t.test.default"]],
#'   arg_names = c("mu", "paired", "var.equal", "conf.level"),
#'   passed_args = list(conf.level = 0.90)
#' )
ard_formals <- function(fun, arg_names, passed_args = list(),
                        envir = parent.frame()) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(fun)
  check_not_missing(arg_names)
  check_class(passed_args, "list")
  check_class(fun, "function")
  check_class(arg_names, "character")
  check_class(envir, "environment")

  # prepare named list of arguments --------------------------------------------
  lst_args <-
    formals(fun = fun, envir = envir)[arg_names] |>
    utils::modifyList(val = passed_args[intersect(arg_names, names(passed_args))], keep.null = TRUE)

  # put formals list in ARD structure ------------------------------------------
  enframe(lst_args[arg_names], "stat_name", "stat") |>
    dplyr::mutate(stat_label = .data$stat_name, .after = "stat_name") |>
    as_card()
}
