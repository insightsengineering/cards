# THIS SCRIPT MUST OPERATE AS A STANDALONE SCRIPT
# DO NOT USE IMPORTED FUNCTIONS AND ONLY USE rlang AND cli NAMESPACING FOR CHECKS


#' Check Class
#'
#' @param class character vector or string indicating accepted classes.
#' Passed to `inherits(what=class)`
#' @param ... named arguments, e.g. `data = data`. The _name_ is used in the error
#' messaging and the passed object's class is checked.
#' @param allow_null Logical indicating whether a NULL value will pass the test.
#' Default is `FALSE`
#' @param length When not NULL, includes check the value is the length specified.
#' @inheritParams cli::cli_abort
#' @keywords internal
check_class <- function(class, ..., allow_null = FALSE, call = parent.frame(), length = NULL) {
  dots <- dots_list(..., .named = TRUE)

  # include NULL class as acceptable if allow_null is TRUE
  if (allow_null) class <- c(class, "NULL")

  # check class
  .mapply(
    FUN = function(x, y) {
      if (!inherits(x, class)) {
        cli::cli_abort("The {.arg {y}} argument must be class {.cls {class}}.", call = call)
      }
    },
    dots = list(dots, names(dots)),
    MoreArgs = NULL
  )

  # check length of input
  if (!is.null(length)) {
    .mapply(
      FUN = function(x, y) {
        # if NULL is allowed and is null, then skip length check
        if (isTRUE(allow_null) && is.null(x)) return(invisible())

        # check length
        if (length(x) != length) {
          cli::cli_abort("The {.arg {y}} argument must be length {.val {length}}.", call = call)
        }
      },
      dots = list(dots, names(dots)),
      MoreArgs = NULL
    )
  }
  invisible()
}

#' Check Class Data Frame
#'
#' @inheritParams check_class
#' @keywords internal
check_class_data_frame <- function(..., allow_null = FALSE, call = parent.frame()) {
  check_class(class = "data.frame", ..., allow_null = allow_null, call = call)
}

#' Check Argument not Missing
#'
#' @param x argument to check
#' @param arg_name string indicating the name of the argument. Used in the error messaging.
#' @inheritParams check_class
#' @keywords internal
check_not_missing <- function(x, arg_name = caller_arg(x), call = parent.frame()) {
  if (missing(x)) {
    cli::cli_abort("The {.arg {arg_name}} argument cannot be missing.", call = call)
  }
  invisible()
}

#' Check Length
#'
#' @param x object to check
#' @param msg string passed to `cli::cli_abort(message=)`
#' @param length integer specifying the required length
#' @keywords internal
check_length <- function(x, arg_name = caller_arg(x), length = 1L,
                         msg = "The {.arg {arg_name}} argument must be length {.val {length}}.",
                         call = parent.frame()) {
  if (length(x) != length) {
    cli::cli_abort(message = msg, call = call)
  }
  invisible()
}

#' Check Range
#'
#' @param x numeric scalar to check
#' @param range numeric vector of length two
#' @param include_bounds logical of length two indicating whether to allow
#'   the lower and upper bounds
#' @param scalar logical indicating whether `x` must be a scalar
#' @param msg string passed to `cli::cli_abort(message=)`
#'
#' @return invisible
#' @keywords internal
check_range <- function(x,
                        range,
                        include_bounds = c(FALSE, FALSE),
                        arg_name = caller_arg(x),
                        scalar = FALSE,
                        msg = paste(
                          "The {.arg {arg_name}} argument must be in the interval",
                          "{.code {ifelse(include_bounds[1], '[', '(')}{range[1]},",
                          "{range[2]}{ifelse(include_bounds[2], ']', ')')}}."),
                        call = parent.frame()) {
  print_error <- FALSE
  # check input is numeric
  if (!is.numeric(x)) {
    print_error <- TRUE
  }

  # check the lower bound of range
  if (isFALSE(print_error) && isTRUE(include_bounds[1]) && any(x < range[1])) {
    print_error <- TRUE
  }
  if (isFALSE(print_error) && isFALSE(include_bounds[1]) && any(x <= range[1])) {
    print_error <- TRUE
  }

  # check upper bound of range
  if (isFALSE(print_error) && isTRUE(include_bounds[2]) && any(x > range[2])) {
    print_error <- TRUE
  }
  if (isFALSE(print_error) && isFALSE(include_bounds[2]) && any(x >= range[2])) {
    print_error <- TRUE
  }

  # print error
  if (print_error) {
    cli::cli_abort(msg, call = call)
  }

  invisible()
}


#' Check Binary
#'
#' Checks if a column in a data frame is binary,
#' that is, if the column is class `<logical>` or
#' `<numeric/integer>` and coded as `c(0, 1)`
#'
#' @param x a vector
#' @param call call environment
#'
#' @return invisible
#' @keywords internal
check_binary <- function(x, arg_name = caller_arg(x), call = parent.frame()) {
  if (!is.logical(x) && !(is_integerish(x) && is_empty(setdiff(x, c(0, 1, NA))))) {
    paste("Expecting column {.arg {arg_name}} to be either {.cls logical}",
          "or {.cls {c('numeric', 'integer')}} coded as {.val {c(0, 1)}}.") |>
      cli::cli_abort(call = call)
  }

  invisible()
}
