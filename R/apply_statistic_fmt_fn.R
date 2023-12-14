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
        map2(
          .data$statistic,
          .data$statistic_fmt_fn,
          function(x, fn) {
            if (!is.null(fn)) do.call(.convert_alias_to_fmt_fn(fn), args = list(x))
            else NULL
          }
        )
    )
}

.convert_alias_to_fmt_fn <- function(x, call = rlang::caller_env()) {
  if (is.function(x))
    return(x)
  if (rlang::is_integerish(x) && x >= 0L)
    return(function(.x) format(round2(.x, digits = as.integer(x)), nsmall = as.integer(x)))
  if (rlang::is_string(x)) {
    .check_fmt_string(x, call = call)
    # scale by 100 if it's a percentage
    scale <- ifelse(endsWith(x, "%"), 100, 1)
    decimal_n <-
      ifelse(
        !grepl("\\.", x),
        0L,
        gsub("%", "", x) |> # remove percent sign if it is there
          strsplit("xx.xx", split = ".", fixed = TRUE) |> # split string at decimal place
          unlist() %>%
          `[`(2) %>% # get the string after the period
          {ifelse(is.na(.), 0L, nchar(.))}
      )
    width <- nchar(x) - endsWith(x, "%")

    fn <- function(y) {
      fmt <- format(round2(y * scale, digits = decimal_n), nsmall = decimal_n)
      if (nchar(fmt) > width) {
        cli::cli_warn("Formatted statistic, {.val {fmt}}, is longer than allowed by format {.val {x}}", call = call)
        return(fmt)
      }
      paste0(strrep(" ", width - nchar(fmt)), fmt)
    }

    return(fn)
  }

  cli::cli_abort("Formatting functions/aliases must be a function, a non-negative integer, or a formatting string, e.g. {.val xx.x}.", call = call)
}


#' Check 'xx' Format Structure
#'
#' @description
#' A function that checks a **single** string for consistency.
#' String must begin with 'x' and only consist of x's, a single period or none,
#' and may end with a percent symbol.
#'
#' If string is consistent, `TRUE` is returned. Otherwise an error.
#'
#'
#' @param x string to check
#' @param call calling environment. Default is `rlang::caller_env()`
#'
#' @return logical
#' @keywords internal
#'
#' @examples
#' cards:::.check_fmt_string("xx.x")  # TRUE
#' cards:::.check_fmt_string("xx.x%") # TRUE
.check_fmt_string <- function(x, call = rlang::caller_env()) {
  # perform checks on the string
  fmt_is_good <-
    grepl("^x[x.%]+$", x = x) && # string begins with 'x', and consists of only x, period, or percent
    sum(unlist(gregexpr("\\.", x)) != -1) %in% c(0L, 1L) && # a period appears 0 or 1 times
    sum(unlist(gregexpr("%", x)) != -1) %in% c(0L, 1L) && # a percent appears 0 or 1 times
    (sum(unlist(gregexpr("%", x)) != -1) %in% 0L || grepl(pattern = "%$", x = x)) # if there is a % it appears at the end

  if (isFALSE(fmt_is_good)) {
    cli::cli_abort("The format {.val {x}} is not valid.", call = call)
  }
  fmt_is_good
}
