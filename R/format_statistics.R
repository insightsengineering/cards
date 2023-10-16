
format_statistics <- function(x, verbose = FALSE) {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "card")) {
    cli::cli_abort("Argument {.code x} must be class {.cls card}")
  }
  required_cols <- c("statistic_fmt_fn", "statistic")
  if (any(!required_cols %in% colnames(x))) {
    if (verbose) {
      cli::cli_inform("Columns {.val {required_cols}} are required to format statistics.")
    }
    return(x)
  }

  # add column with formatted statistics ---------------------------------------
  x$statistics_formatted <-
    map2(
      x$statistic,
      x$statistic_fmt_fn,
      function(x, fn) do.call(fn[[1]], args = list(x[[1]]))
    )

  # return ARD with new formatted statistics column ----------------------------
  x
}
