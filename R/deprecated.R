#' Deprecated functions
#'
#' `r lifecycle::badge('deprecated')`\cr
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

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
