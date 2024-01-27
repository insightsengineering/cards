# DO NOT MODIFY THIS FILE. INSTEAD MODIFY THE VERSION IN https://github.com/ddsjoberg/standalone/tree/main/R
# ---
# repo: ddsjoberg/standalone
# file: standalone-checks.R
# last-updated: 2024-01-24
# license: https://unlicense.org
# imports: rlang, cli
# ---
#
# This file provides a minimal shim to provide a purrr-like API on top of
# base R functions. They are not drop-in replacements but allow a similar style
# of programming.
#
# ## Changelog
# nocov start


#' Check Class
#'
#' @param class (`character`)\cr
#'   character vector or string indicating accepted classes.
#'   Passed to `inherits(what=class)`
#' @param x `(object)`\cr
#'   object to check
#' @param allow_null (`logical(1)`)\cr
#'   Logical indicating whether a NULL value will pass the test.
#'   Default is `FALSE`
#' @param arg_name (`string`)\cr
#'   string indicating the label/symbol of the object being checked.
#'   Default is `rlang::caller_arg(x)`
#' @inheritParams cli::cli_abort
#' @keywords internal
#' @noRd
check_class <- function(x, class, allow_null = FALSE,
                        arg_name = rlang::caller_arg(x), call = parent.frame()) {
  # include NULL class as acceptable if allow_null is TRUE
  if (isTRUE(allow_null) && is.null(x)) {
    return(invisible())
  }

  if (!inherits(x, class)) {
    cli::cli_abort("The {.arg {arg_name}} argument must be class {.cls {class}}.", call = call)
  }
  invisible()
}

#' Check Class Data Frame
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_class_data_frame <- function(x, allow_null = FALSE,
                                   arg_name = rlang::caller_arg(x), call = parent.frame()) {
  check_class(
    x = x, class = "data.frame", allow_null = allow_null,
    arg_name = arg_name, call = call
  )
}

#' Check Argument not Missing
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_not_missing <- function(x, arg_name = rlang::caller_arg(x), call = parent.frame()) {
  if (missing(x)) {
    cli::cli_abort("The {.arg {arg_name}} argument cannot be missing.", call = call)
  }
  invisible()
}

#' Check Length
#'
#' @param msg (`string`)\cr
#'   string passed to `cli::cli_abort(message=)`
#' @param length (`integer(1)`)\cr
#'   integer specifying the required length
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_length <- function(x, length, arg_name = rlang::caller_arg(x), call = parent.frame()) {
  if (length(x) != length) {
    cli::cli_abort("The {.arg {arg_name}} argument must be length {.val {length}}.", call = call)
  }
  invisible()
}

#' Check is Scalar
#'
#' @param msg (`string`)\cr
#'   string passed to `cli::cli_abort(message=)`
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_scalar <- function(x, arg_name = rlang::caller_arg(x), call = parent.frame()) {
  check_length(x = x, length = 1L, arg_name = arg_name, call = call)
}

# nocov end
