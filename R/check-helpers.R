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
#' @inheritParams cli::cli_abort
#' @keywords internal
check_class <- function(class, ..., allow_null = FALSE, call = parent.frame()) {
  dots <- rlang::dots_list(..., .named = TRUE)

  # include NULL class as acceptable if allow_null is TRUE
  if (allow_null) class <- c(class, "NULL")

  .mapply(
    FUN = function(x, y) {
      if (!inherits(x, class)) {
        cli::cli_abort("The {.arg {y}} argument must be class {.cls {class}}.", call = call)
      }
    },
    dots = list(dots, names(dots)),
    MoreArgs = NULL
  )
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
check_not_missing <- function(x, arg_name, call = parent.frame()) {
  if (missing(x)) {
    cli::cli_abort("The {.arg {arg_name}} argument cannot be missing.", call = call)
  }
  invisible()
}

#' Check Columns in Data Frame
#'
#' @param data a data frame
#' @param columns character vector of column names to check
#' @param msg string passed to `cli::cli_abort(message=)`
#' @inheritParams check_class
#' @keywords internal
check_columns_in_data_frame <- function(data, columns,
                                        msg = "Columns {.val {missing_cols}} not found in {.arg data}.",
                                        call = parent.frame()) {
  missing_cols <- columns |> setdiff(names(data))
  if (!rlang::is_empty(missing_cols)) {
    cli::cli_abort(message = msg, call = call)
  }
  invisible()
}

#' Check Length
#'
#' @param x object to check
#' @param msg string passed to `cli::cli_abort(message=)`
#' @param length integer specifying the required length
#' @keywords internal
check_length <- function(x, arg_name, length = 1L,
                                        msg = "The {.arg by} argument must be length {.val {length}}.",
                                        call = parent.frame()) {
  if (length(x) != length) {
    cli::cli_abort(message = msg, call = call)
  }
  invisible()
}
