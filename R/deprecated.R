#' Deprecated functions
#'
#' `r lifecycle::badge('deprecated')`\cr
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

# "soft" deprecation for 6 months: (Sys.Date() - lubridate::dmonths(6)) |> as.Date()
#  v0.5.1 2025-03-01
#  v0.5.0 2025-02-17

# "warn" deprecation for 12 months: (Sys.Date() - lubridate::dmonths(12)) |> as.Date()

# "stop" deprecation for 18 months: (Sys.Date() - lubridate::dmonths(18)) |> as.Date()

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
