#' Apply Formatting Functions
#'
#' Apply the formatting functions to each of the raw statistics.
#' Function aliases are converted to functions using [alias_as_fmt_fn()].
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param replace (scalar `logical`)\cr
#'   logical indicating whether to replace values in the `'stat_fmt'` column (if present).
#'   Default is `FALSE`.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_continuous(ADSL, variables = "AGE") |>
#'   apply_fmt_fn()
apply_fmt_fn <- function(x, replace = FALSE) {
  set_cli_abort_call()

  check_class(x, cls = "card")
  check_scalar_logical(replace)

  # add stat_fmt if not already present, if replace is TRUE overwrite existing stat_fmt column
  if (!"stat_fmt" %in% names(x) || isTRUE(replace)) {
    x <- x |> dplyr::mutate(.after = "stat", stat_fmt = list(NULL))
  }

  x |>
    dplyr::mutate(
      stat_fmt =
        pmap(
          list(
            .data$stat,
            .data$variable,
            .data$stat_name,
            .data$fmt_fn,
            .data$stat_fmt
          ),
          function(stat, variable, stat_name, fn, stat_fmt) {
            if (!is.null(fn) && is.null(stat_fmt)) {
              tryCatch(
                do.call(alias_as_fmt_fn(fn, variable, stat_name), args = list(stat)),
                error = \(e) {
                  cli::cli_abort(
                    c("There was an error applying the formatting function to
                       statistic {.val {stat_name}} for variable {.val {variable}}.",
                      "i" = "Perhaps try formmatting function {.fun as.character}? See error message below:",
                      "x" = conditionMessage(e)
                    ),
                    call = get_cli_abort_call()
                  )
                }
              )
            } else {
              stat_fmt
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
#' @param variable (`character`)\cr the variable whose statistic is to be formatted
#' @param stat_name (`character`)\cr the name of the statistic that is to be formatted
#'
#' @return a function
#' @export
#'
#' @examples
#' alias_as_fmt_fn(1)
#' alias_as_fmt_fn("xx.x")
alias_as_fmt_fn <- function(x, variable, stat_name) {
  set_cli_abort_call()

  if (is.function(x)) {
    return(x)
  }
  if (is_integerish(x) && x >= 0L) {
    return(label_round(digits = as.integer(x)))
  }
  if (is_string(x)) {
    .check_fmt_string(x, variable, stat_name)
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

    return(label_round(digits = decimal_n, scale = scale, width = width))
  }

  # if the above conditions are not met, return an error -----------------------
  if (!missing(variable) && !missing(stat_name)) {
    error_message <-
      c("The value in {.arg fmt_fn} cannot be converted into a function for
         statistic {.val {stat_name}} and variable {.val {variable}}.",
        "i" = "Value must be a function, a non-negative integer, or a formatting string, e.g. {.val xx.x}.",
        "*" = "See {.help cards::alias_as_fmt_fn} for details."
      )
  } else {
    error_message <-
      c("The value in {.arg fmt_fn} cannot be converted into a function.",
        "i" = "Value must be a function, a non-negative integer, or a formatting string, e.g. {.val xx.x}.",
        "*" = "See {.help cards::alias_as_fmt_fn} for details."
      )
  }

  cli::cli_abort(
    message = error_message,
    call = get_cli_abort_call()
  )
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
#' label_round(2)(pi)
#' label_round(1, scale = 100)(pi)
#' label_round(2, width = 5)(pi)
label_round <- function(digits = 1, scale = 1, width = NULL) {
  round_fun <- .get_round_fun()

  function(x) {
    # round and scale vector
    res <-
      ifelse(
        is.na(x),
        NA_character_,
        format(round_fun(x * scale, digits = digits), nsmall = digits) |> str_trim()
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

.get_round_fun <- function() {
  switch(getOption("cards.round_type", default = "round-half-up"),
    "round-half-up" = round5,
    "round-to-even" = round
  ) %||%
    cli::cli_abort(
      "The {.arg cards.round_type} {.emph option} must be one of
         {.val {c('round-half-up', 'round-to-even')}}.",
      call = get_cli_abort_call()
    )
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
#' @param variable (`character`)\cr the variable whose statistic is to be formatted
#' @param stat_name (`character`)\cr the name of the statistic that is to be formatted
#'
#' @return a logical
#' @keywords internal
#'
#' @examples
#' cards:::.check_fmt_string("xx.x") # TRUE
#' cards:::.check_fmt_string("xx.x%") # TRUE
.check_fmt_string <- function(x, variable, stat_name) {
  set_cli_abort_call()

  # perform checks on the string
  fmt_is_good <-
    grepl("^x[x.%]+$", x = x) && # string begins with 'x', and consists of only x, period, or percent
      sum(unlist(gregexpr("\\.", x)) != -1) %in% c(0L, 1L) && # a period appears 0 or 1 times
      sum(unlist(gregexpr("%", x)) != -1) %in% c(0L, 1L) && # a percent appears 0 or 1 times
      (sum(unlist(gregexpr("%", x)) != -1) %in% 0L || grepl(pattern = "%$", x = x)) # if there is a % it appears at the end

  if (isFALSE(fmt_is_good)) {
    cli::cli_abort(
      message =
        "The format {.val {x}} for `fmt_fn` is not valid for the
         variable {.val {variable}} for the statistic {.val {stat_name}}.
         String must begin with 'x' and only consist of x's, a single period or
         none, and may end with a percent symbol.",
      call = get_cli_abort_call()
    )
  }
  fmt_is_good
}
