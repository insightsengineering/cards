#' Proportion ARD Statistics
#'
#' Calculate confidence intervals for proportions.
#'
#' @inheritParams ard_categorical
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to include in summaries. Columns must be class `<logical>`
#'   or `<numeric>` values coded as `c(0, 1)`.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to stratify calculations by
#' @param conf.level (`numeric`)\cr
#'   a scalar in `(0, 1)` indicating the confidence level.
#'   Default is `0.95`
#' @param method (`string`)\cr
#'   string indicating the type of confidence interval to calculate.
#'   Must be one of `r formals(ard_proportion_ci)[["method"]] |> eval() |> shQuote()`.
#'
#' @return an ARD data frame
#' @export
#'
#' @examples
#' ard_proportion_ci(mtcars, variables = c(vs, am), method = "wilson")
ard_proportion_ci <- function(data, variables, by = dplyr::group_vars(data),
                              conf.level = 0.95,
                              method = c("waldcc", "wald", "clopper-pearson",
                                         "wilson", "wilsoncc",
                                         "agresti-coull", "jeffreys")) {
  # process inputs -------------------------------------------------------------
  process_selectors(data, variables = {{  variables }}, by = {{  by }})
  method <- arg_match(method)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE), scalar = TRUE)
  # check variables are binary
  walk(variables, ~.is_binary(data, variable = .x))

  ard_continuous(
    data = data,
    variables = {{ variables }},
    by = {{ by }},
    statistics =
      ~list(
        prop_ci =
          switch(
            method,
            "waldcc" = \(x) proportion_ci_wald(x, conf.level = conf.level, correct = TRUE),
            "wald" = \(x) proportion_ci_wald(x, conf.level = conf.level, correct = FALSE),
            "wilsoncc" = \(x) proportion_ci_wilson(x, conf.level = conf.level, correct = TRUE),
            "wilson" = \(x) proportion_ci_wilson(x, conf.level = conf.level, correct = FALSE),
            "clopper-pearson" = \(x) proportion_ci_clopper_pearson(x, conf.level = conf.level),
            "agresti-coull" = \(x) proportion_ci_agresti_coull(x, conf.level = conf.level),
            "jeffreys" = \(x) proportion_ci_jeffreys(x, conf.level = conf.level)
          )
      )
  ) |>
    dplyr::mutate(
      context = "proportion_ci"
    )
}


#' Is Binary?
#'
#' Checks if a column in a data frame is binary,
#' that is, if the column is class `<logical>` or
#' numeric and coded as `c(0, 1)`
#'
#' @param data a data frame
#' @param variable `<string>`\cr a column name from `data`
#' @param call call environment
#'
#' @return invisible
#' @keywords internal
.is_binary <- function(data, variable, call = caller_env()) {
  if (!is.logical(data[[variable]]) && !is_empty(setdiff(data[[variable]], c(0, 1, NA)))) {
    paste("Expecting column {.val {variable}} to be either {.cls logical}",
          "or {.cls numeric} coded as {.val {c(0, 1)}}.") |>
      cli::cli_abort(call = call)
  }
}


#' Helper Functions for Calculating Proportion Confidence Intervals
#'
#' Functions to calculate different proportion confidence intervals for use in `ard_proportion()`.
#'
#' @inheritParams ard_proportion_ci
#' @param x vector of a binary values, i.e. a logical vector, or numeric with values `c(0, 1)`
#' @return Confidence interval of a proportion.
#'
#' @name proportion_ci
#' @examples
#' x <- c(TRUE, TRUE, TRUE, TRUE, TRUE,
#'          FALSE, FALSE, FALSE, FALSE, FALSE)
#'
#' proportion_ci_wald(x, conf.level = 0.9)
#' proportion_ci_wilson(x, correct = TRUE)
#' proportion_ci_clopper_pearson(x)
#' proportion_ci_agresti_coull(x)
#' proportion_ci_jeffreys(x)
NULL

#' @describeIn proportion_ci Calculates the Wald interval by following the usual textbook definition
#'   for a single proportion confidence interval using the normal approximation.
#'
#' @param correct (`logical`)\cr apply continuity correction.
#'
#' @export
proportion_ci_wald <- function(x, conf.level = 0.95, correct = FALSE) {
  x <- stats::na.omit(x)

  n <- length(x)
  p_hat <- mean(x)
  z <- stats::qnorm((1 + conf.level) / 2)
  q_hat <- 1 - p_hat
  correction_factor <- ifelse(correct, 1 / (2 * n), 0)

  err <- z * sqrt(p_hat * q_hat) / sqrt(n) + correction_factor
  l_ci <- max(0, p_hat - err)
  u_ci <- min(1, p_hat + err)

  list(
    N = n,
    p = p_hat,
    conf.low = l_ci,
    conf.high = u_ci,
    conf.level = conf.level,
    method =
      glue::glue("Wald Confidence Interval {ifelse(correct, 'with', 'without')} continuity correction")
  )
}


#' @describeIn proportion_ci Calculates the Wilson interval by calling [stats::prop.test()].
#'  Also referred to as Wilson score interval.
#'
#' @export
proportion_ci_wilson <- function(x, conf.level = 0.95, correct = FALSE) {
  x <- stats::na.omit(x)

  n <- length(x)
  y <- stats::prop.test(x = sum(x), n = n, correct = correct, conf.level = conf.level)

  list(
    N = n,
    p = mean(x),
    conf.low = y$conf.int[1],
    conf.high = y$conf.int[2],
    conf.level = conf.level,
    method =
      glue::glue("Wilson Confidence Interval {ifelse(correct, 'with', 'without')} continuity correction")
  )
}

#' @describeIn proportion_ci Calculates the Clopper-Pearson interval by calling [stats::binom.test()].
#'   Also referred to as the `exact` method.
#' @export
proportion_ci_clopper_pearson <- function(x, conf.level = 0.95) {
  x <- stats::na.omit(x)
  n <- length(x)

  y <- stats::binom.test(x = sum(x), n = n, conf.level = conf.level)

  list(
    N = n,
    p = mean(x),
    conf.low = y$conf.int[1],
    conf.high = y$conf.int[2],
    conf.level = conf.level,
    method =
      glue::glue("Clopper-Pearson Confidence Interval")
  )
}

#' @describeIn proportion_ci Calculates the `Agresti-Coull` interval (created by `Alan Agresti` and `Brent Coull`) by
#'   (for 95% CI) adding two successes and two failures to the data and then using the Wald formula to construct a CI.
#' @export
proportion_ci_agresti_coull <- function(x, conf.level = 0.95) {
  x <- stats::na.omit(x)

  n <- length(x)
  x_sum <- sum(x)
  z <- stats::qnorm((1 + conf.level) / 2)

  # Add here both z^2 / 2 successes and failures.
  x_sum_tilde <- x_sum + z^2 / 2
  n_tilde <- n + z^2

  # Then proceed as with the Wald interval.
  p_tilde <- x_sum_tilde / n_tilde
  q_tilde <- 1 - p_tilde
  err <- z * sqrt(p_tilde * q_tilde) / sqrt(n_tilde)
  l_ci <- max(0, p_tilde - err)
  u_ci <- min(1, p_tilde + err)

  list(
    N = n,
    p = mean(x),
    conf.low = l_ci,
    conf.high = u_ci,
    conf.level = conf.level,
    method = glue::glue("Agresti-Coull Confidence Interval")
  )
}

#' @describeIn proportion_ci Calculates the Jeffreys interval, an equal-tailed interval based on the
#'   non-informative Jeffreys prior for a binomial proportion.
#' @export
proportion_ci_jeffreys <- function(x, conf.level = 0.95) {
  x <- stats::na.omit(x)

  n <- length(x)
  x_sum <- sum(x)

  alpha <- 1 - conf.level
  l_ci <- ifelse(
    x_sum == 0,
    0,
    stats::qbeta(alpha / 2, x_sum + 0.5, n - x_sum + 0.5)
  )

  u_ci <- ifelse(
    x_sum == n,
    1,
    stats::qbeta(1 - alpha / 2, x_sum + 0.5, n - x_sum + 0.5)
  )

  list(
    N = n,
    p = mean(x),
    conf.low = l_ci,
    conf.high = u_ci,
    conf.level = conf.level,
    method = glue::glue("Jeffreys Interval")
  )
}

