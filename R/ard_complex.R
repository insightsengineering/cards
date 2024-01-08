#' Complex ARD Summaries
#'
#' `r lifecycle::badge('experimental')`\cr
#' Function is similar to `ard_continuous()`, but allows for more complex
#' summaries. While `ard_continuous(statistics)` only allows for a univariable
#' function, `ard_complex(statistics)` can handle more complex data summaries.
#'
#' @inheritParams ard_continuous
#' @param statistics ([`formula-list-selector`][syntax])\cr
#'   The form of the statistics argument is identical to `ard_continuous(statistics)`
#'   argument, except the summary function _must_ accept the following arguments.
#'   - `x` vector
#'   - `data` the data frame that has been subset by the `by`/`strata` columns
#'       and rows in which `"variable"` is `NA` have been removed.
#'   - `data_full` the full data frame
#'   - `by` character vector of the `by` variables
#'   - `strata` character vector of the `strata` variables
#'   It is unlikely any one function will need _all_ of the above elements,
#'   and it's recommended the function passed accepts `...` and any unused
#'   arguments will be properly ignored. The `...` also allows this function
#'   to perhaps be updated in the future with more passed arguments. For example,
#'   if one needs a second variable from the data frame, the function inputs
#'   may look like: `foo(x, data, ...)`
#'
#' @return a data frame
#' @export
#'
#' @examples
#' # example how to mimic behavior of `ard_continuous()`
#' ard_complex(
#'   ADSL,
#'   by = "ARM",
#'   variables = "AGE",
#'   statistics = list(AGE = list(mean = \(x, ...) mean(x)))
#' )
#'
#' # return the grand mean and the mean within the by group
#' grand_mean <- function(data, data_full, variable, ...) {
#'   list(mean = mean(data[[variable]], na.rm = TRUE),
#'        grand_mean = mean(data_full[[variable]], na.rm = TRUE))
#' }
#'
#' ard_complex(
#'   ADSL,
#'   by = "ARM",
#'   variables = "AGE",
#'   statistics = list(AGE = list(means = grand_mean))
#' )
ard_complex <- function(data,
                        variables,
                        by = NULL,
                        strata = NULL,
                        statistics,
                        fmt_fn = NULL,
                        stat_labels = everything() ~ default_stat_labels()) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(statistics)
  check_class_data_frame(data = data)
  check_class(class = c("list", "formula"), statistics = statistics, allow_null = FALSE)

  # process inputs -------------------------------------------------------------
  process_selectors(data, variables = variables)
  process_formula_selectors(data[variables], statistics = statistics)

  missing_statistics_vars <- setdiff(variables, names(statistics))
  if (!is_empty(missing_statistics_vars)) {
    "The following columns do not have {.arg statistics} defined: {.val {missing_statistics_vars}}." |>
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
    statistics = statistics,
    fmt_fn = fmt_fn,
    stat_labels = stat_labels
  ) |>
    dplyr::mutate(context = "complex")

}
