#' Apply Formatting Functions
#'
#' Apply the formatting functions to each of the raw statistics.
#'
#' @param x an ARD data frame
#'
#' @return an ARD data frame
#' @export
#'
#' @examples
#' ard_continuous(ADSL, variables = "AGE") |>
#'   apply_statistic_fmt_fn()
apply_statistic_fmt_fn <- function(x) {
  if (!inherits(x, "card")) {
    cli::cli_abort(c("i" = "Argument {.code x} must be class {.cls card}."))
  }

  x |>
    dplyr::mutate(
      .after = "statistic",
      statistic_fmt =
        .map2(
          .data$statistic,
          .data$statistic_fmt_fn,
          function(x, fn) if (!is.null(fn)) fn(x) else NULL
        )
    )
}
