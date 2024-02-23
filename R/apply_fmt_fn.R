#' Apply Formatting Functions
#'
#' Apply the formatting functions to each of the raw statistics.
#' Function aliases are converted to functions using [alias_as_fmt_fn()].
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_continuous(ADSL, variables = "AGE") |>
#'   apply_fmt_fn()
apply_fmt_fn <- function(x) {
  if (!inherits(x, "card")) {
    cli::cli_abort(c("i" = "Argument {.code x} must be class {.cls card}."))
  }

  x |>
    dplyr::mutate(
      .after = "stat",
      stat_fmt =
        map2(
          .data$stat,
          .data$fmt_fn,
          function(x, fn) {
            if (!is.null(fn)) {
              do.call(alias_as_fmt_fn(fn), args = list(x))
            } else {
              NULL
            }
          }
        )
    )
}



#' Convert Alias to Function
#'
#' @description
#' Accepted aliases are non-negative integers and strings.
#'
#' The integers are converted to functions that round the statistics
#' to the number of decimal places to match the integer.
#'
#' The formatting strings come in the form `"xx"`, `"xx.x"`, `"xx.x%"`, etc.
#' The number of `x`s that appear after the decimal place indicate the number of
#' decimal places the statistics will be rounded to.
#' The number of `x`s that appear before the decimal place indicate the leading
#' spaces that are added to the result.
#' If the string ends in `"%"`, results are scaled by 100 before rounding.
#'
#' @param x (`integer`, `string`, or `function`)\cr
#'   a non-negative integer, string alias, or function
#' @param call (`environment`)\cr
#'   frame for error messaging. Default is [parent.frame()].
#'
#' @return a function
#' @export
#'
#' @examples
#' alias_as_fmt_fn(1)
#' alias_as_fmt_fn("xx.x")
alias_as_fmt_fn <- function(x, call = parent.frame()) {
  if (is.function(x)) {
    return(x)
  }
  if (is_integerish(x) && x >= 0L) {
    return(label_cards(digits = as.integer(x)))
  }
  if (is_string(x)) {
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
          {ifelse(is.na(.), 0L, nchar(.))} # styler: off
      )
    width <- nchar(x) - endsWith(x, "%")

    return(label_cards(digits = decimal_n, scale = scale, width = width))
  }

  cli::cli_abort("Formatting functions/aliases must be a function, a non-negative integer, or a formatting string, e.g. {.val xx.x}.", call = call)
}


#' Generate Formatting Function
#'
#' Returns a function with the requested rounding and scaling schema.
#'
#' @param digits (`integer`)\cr
#'   a non-negative integer specifying the number of decimal places
#'   round statistics to
#' @param scale (`numeric`)\cr
#'   a scalar real number. Before rounding, the input will be scaled by
#'   this quantity
#' @param width (`integer`)\cr
#'   a non-negative integer specifying the minimum width of the
#'   returned formatted values
#'
#' @return a function
#' @export
#'
#' @examples
#' label_cards(2)(pi)
#' label_cards(1, scale = 100)(pi)
#' label_cards(2, width = 5)(pi)
label_cards <- function(digits = 1, scale = 1, width = NULL) {
  function(x) {
    # round and scale vector
    res <-
      ifelse(
        is.na(x),
        NA_character_,
        format(round5(x * scale, digits = digits), nsmall = digits) |> str_trim()
      )


    # if width provided, pad formatted result
    if (!is.null(width)) {
      res <-
        ifelse(
          nchar(res) >= width | is.na(res),
          res,
          paste0(strrep(" ", width - nchar(res)), res)
        )
    }

    # return final formatted vector
    res
  }
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
#' @param x (`string`)\cr
#'   string to check
#' @param call (`environment`)\cr
#'   frame for error messaging. Default is [caller_env()].
#'
#' @return a logical
#' @keywords internal
#'
#' @examples
#' cards:::.check_fmt_string("xx.x") # TRUE
#' cards:::.check_fmt_string("xx.x%") # TRUE
#' @noRd
.check_fmt_string <- function(x, call = caller_env()) {
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
