#' Options in \{cards\}
#'
#' @name cards.options
#' @description
#' See below for options available in the \{cards\} package
#'
#' @section cards.round_type:
#' There are two types of rounding types in the \{cards\} package that are implemented
#' in `label_round()`, `alias_as_fmt_fn()`, and `apply_fmt_fn()` functions' `round_type`
#' argument.
#'
#' - `'round-half-up'` (_default_): rounding method where values exactly halfway
#'    between two numbers are rounded to the larger in magnitude number.
#'    Rounding is implemented via [`round5()`].
#' - `'round-to-even'`: base R's default IEC 60559 rounding standard.
#'    See [`round()`] for details.
#'
#' To change the default rounding to use IEC 60559, this option must be set **both**
#' when the ARDs are created and when `apply_fmt_fn()` is run. This ensures that
#' any _default_ formatting functions created with `label_round()` utilize the
#' specified rounding method and the method is used what aliases are converted
#' into functions (which occurs in `apply_fmt_fn()` when it calls `alias_as_fmt_fn()`).
NULL
