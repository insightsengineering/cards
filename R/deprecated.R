#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge('deprecated')`\cr
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' **Renamed functions**
#'  - `ard_categorical()` to `ard_tabulate()`
#'  - `ard_continuous()` to `ard_summary()`
#'  - `ard_complex()` to `ard_mvsummary()`
#'  - `apply_fmt_fn()` to `apply_fmt_fun()`
#'  - `alias_as_fmt_fn()` to `alias_as_fmt_fun()`
#'  - `update_ard_fmt_fn()` to `update_ard_fmt_fun()`
#'
#' @name deprecated
#' @keywords internal
NULL

# "soft" deprecation for 6 months: (Sys.Date() - lubridate::dmonths(6)) |> as.Date()
#  v0.5.1 2025-03-01
#  v0.5.0 2025-02-17

# "warn" deprecation for 12 months: (Sys.Date() - lubridate::dmonths(12)) |> as.Date()

# "stop" deprecation for 18 months: (Sys.Date() - lubridate::dmonths(18)) |> as.Date()

# v0.7.0 -----------------------------------------------------------------------
# These were dropped from the documentation in v0.7.0. But were not officially deprecated
#' @rdname deprecated
#' @export
ard_continuous <- function(data, ...) {
  check_not_missing(data)
  UseMethod("ard_continuous")
}

#' @rdname deprecated
#' @export
ard_categorical <- function(data, ...) {
  check_not_missing(data)
  UseMethod("ard_categorical")
}

#' @rdname deprecated
#' @export
ard_complex <- function(data, ...) {
  check_not_missing(data)
  UseMethod("ard_complex")
}

#' @rdname deprecated
#' @export
ard_dichotomous <- function(data, ...) {
  check_not_missing(data)
  UseMethod("ard_dichotomous")
}

#' @rdname deprecated
#' @export
ard_continuous.data.frame <- function(data, ...) {
  ard_summary(data = data, ...) |>
    dplyr::mutate(context = "continuous")
}

#' @rdname deprecated
#' @export
ard_categorical.data.frame <- function(data, ...) {
  ard_tabulate(data = data, ...) |>
    dplyr::mutate(context = "categorical")
}

#' @rdname deprecated
#' @export
ard_complex.data.frame <- function(data, ...) {
  ard_mvsummary(data = data, ...) |>
    dplyr::mutate(context = "complex")
}

#' @rdname deprecated
#' @export
ard_dichotomous.data.frame <- function(data, variables, value = maximum_variable_value(data[variables]), ...) {
  cards::process_selectors(data, variables = {{ variables }})
  cards::process_formula_selectors(data[variables], value = value)
  fill_formula_selectors(
    data[variables],
    value = formals(asNamespace("cards")[["ard_dichotomous.data.frame"]])[["value"]] |> eval()
  )

  ard_tabulate(
    data = data,
    variables = {{ variables }},
    value = value,
    ...
  ) |>
    dplyr::mutate(context = "dichotomous")
}

# v0.6.1 -----------------------------------------------------------------------
#' @rdname deprecated
#' @export
apply_fmt_fn <- function(...) {
  lifecycle::deprecate_soft(
    when = "0.6.1",
    what = "cards::apply_fmt_fn()",
    with = "apply_fmt_fun()"
  )

  apply_fmt_fun(...)
}

#' @rdname deprecated
#' @export
alias_as_fmt_fn <- function(...) {
  lifecycle::deprecate_soft(
    when = "0.6.1",
    what = "cards::alias_as_fmt_fn()",
    with = "alias_as_fmt_fun()"
  )

  alias_as_fmt_fun(...)
}

#' @rdname deprecated
#' @export
update_ard_fmt_fn <- function(...) {
  lifecycle::deprecate_soft(
    when = "0.6.1",
    what = "cards::update_ard_fmt_fn()",
    with = "update_ard_fmt_fun()"
  )

  alias_as_fmt_fun(...)
}

# Deprecated on 2025-02-08: I don't think any users would have utilized this function, and we can do a quick deprecation cycle.
#' @rdname deprecated
#' @export
label_cards <- function(...) {
  lifecycle::deprecate_soft(
    when = "0.5.0",
    what = "cards::label_cards()",
    with = "label_round()"
  )

  label_round(...)
}
