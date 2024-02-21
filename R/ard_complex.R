#' Complex ARD Summaries
#'
#' `r lifecycle::badge('experimental')`\cr
#' Function is similar to [ard_continuous()], but allows for more complex
#' summaries. While `ard_continuous(statistic)` only allows for a univariable
#' function, `ard_complex(statistic)` can handle more complex data summaries.
#'
#' @inheritParams ard_continuous
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   The form of the statistics argument is identical to `ard_continuous(statistic)`
#'   argument, except the summary function _must_ accept the following arguments:
#'   - `x`: a vector
#'   - `data`: the data frame that has been subset such that the `by`/`strata` columns
#'       and rows in which `"variable"` is `NA` have been removed.
#'   - `data_full`: the full data frame
#'   - `by`: character vector of the `by` variables
#'   - `strata`: character vector of the `strata` variables
#'   It is unlikely any one function will need _all_ of the above elements,
#'   and it's recommended the function passed accepts `...` so that any unused
#'   arguments will be properly ignored. The `...` also allows this function
#'   to perhaps be updated in the future with more passed arguments. For example,
#'   if one needs a second variable from the data frame, the function inputs
#'   may look like: `foo(x, data, ...)`
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' # example how to mimic behavior of `ard_continuous()`
#' ard_complex(
#'   ADSL,
#'   by = "ARM",
#'   variables = "AGE",
#'   statistic = list(AGE = list(mean = \(x, ...) mean(x)))
#' )
#'
#' # return the grand mean and the mean within the `by` group
#' grand_mean <- function(data, data_full, variable, ...) {
#'   list(
#'     mean = mean(data[[variable]], na.rm = TRUE),
#'     grand_mean = mean(data_full[[variable]], na.rm = TRUE)
#'   )
#' }
#'
#' ADSL |>
#'   dplyr::group_by(ARM) |>
#'   ard_complex(
#'     variables = "AGE",
#'     statistic = list(AGE = list(means = grand_mean))
#'   )
ard_complex <- function(data,
                        variables,
                        by = dplyr::group_vars(data),
                        strata = NULL,
                        statistic,
                        fmt_fn = NULL,
                        stat_label = everything() ~ default_stat_labels()) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(statistic)
  check_data_frame(x = data)

  # process inputs -------------------------------------------------------------
  process_selectors(data, variables = {{ variables }})
  process_formula_selectors(data[variables], statistic = statistic, allow_empty = FALSE)

  missing_statistics_vars <- setdiff(variables, names(statistic))
  if (!is_empty(missing_statistics_vars)) {
    "The following columns do not have {.arg statistic} defined: {.val {missing_statistics_vars}}." |>
      cli::cli_abort()
  }

  # calculate statistics -------------------------------------------------------
  # first set an option to be used internally within `ard_continuous()`
  # to calculate the statistics and pass multiple arguments to the
  # user-supplied functions in the `statistics` argument
  old_option <- getOption("cards.calculate_stats_as_ard.eval_fun")
  on.exit(options(cards.calculate_stats_as_ard.eval_fun = old_option), add = TRUE)
  options(
    cards.calculate_stats_as_ard.eval_fun =
    # putting the expr in quotes to avoid note about global variables
      "do.call(fun, args = list(x = stats::na.omit(nested_data[[variable]]),
                                data = tidyr::drop_na(nested_data, any_of(variable)),
                                data_full = data,
                                variable = variable,
                                by = by,
                                strata = strata))" |>
        parse_expr()
  )

  ard_continuous(
    data = data,
    variables = all_of(variables),
    by = {{ by }},
    strata = {{ strata }},
    statistic = statistic,
    fmt_fn = fmt_fn,
    stat_label = stat_label
  ) |>
    dplyr::mutate(context = "complex")
}
