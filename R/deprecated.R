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
  set_cli_abort_call()
  # not using the cute {lifecycle} functions so we didn't need to add it as a
  # dependency, and I don't think anyone has used this function
  cli::cli_warn(
    c("The {.fun label_cards} function has been renamed to {.fun label_round}.",
      "x" = "Please update your code immediately."
    )
  )

  label_round(...)
}
