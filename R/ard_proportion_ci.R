ard_proportion_ci <- function(data, variables, by = dplyr::group_vars(data),
                              conf.level = 0.95,
                              method = c("waldcc", "wald", "clopper-pearson",
                                         "wilson", "wilsonc", "strat_wilson",
                                         "strat_wilsonc", "agresti-coull", "jeffreys")) {
  # process inputs -------------------------------------------------------------
  process_selectors(data, variables = {{  variables }}, by = {{  by }})
  method <- arg_match(method)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE), scalar = TRUE)
  # check variables are binary
  walk(variable, ~.is_binary(data, variable = .x))


  ard_continuous(
    data = data,
    variables = {{ variables }},
    by = {{ by }},
    statistics =
      ~list(
        prop_ci =
          switch(
            method,
            "waldcc" = \(x) .calc_prop_ci_wald(x, conf.level = conf.level, correct = TRUE),
            "wald" = \(x) .calc_prop_ci_wald(x, conf.level = conf.level, correct = FALSE),
          )
      )
  ) |>
    dplyr::mutate(
      context = "proportion_ci"
    )
}

.is_binary <- function(data, variable, call = caller_env()) {
  if (!is.logical(x) && !is_empty(setdiff(data[[variable]], c(0, 1, NA)))) {
    paste("Expecting column {.val {variable}} to be either {.cls logical}",
          "or {.cls numeric} coded as {.val {c(0, 1)}}.") |>
      cli::cli_abort(call = call)
  }
}


#' Helper Functions for Calculating Proportion Confidence Intervals
#'
#' Functions to calculate different proportion confidence intervals for use in `ard_proportion()`.
#'
#' @return Confidence interval of a proportion.
#'
#' @name calc_prop_ci
#' @keywords internal
NULL

#' @describeIn calc_prop_ci Calculates the Wald interval by following the usual textbook definition
#'   for a single proportion confidence interval using the normal approximation.
#'
#' @param correct (`logical`)\cr apply continuity correction.
#'
#' @keywords internal
.calc_prop_ci_wald <- function(x, conf.level, correct = FALSE) {
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
      glue::glue("Wald Confidence Interval {ifelse(correct, 'with', 'without')} continuity correction.")
  )
}


#' @describeIn h_proportions Calculates the Wilson interval by calling [stats::prop.test()].
#'  Also referred to as Wilson score interval.
#'
#' @examples
#' rsp <- c(
#'   TRUE, TRUE, TRUE, TRUE, TRUE,
#'   FALSE, FALSE, FALSE, FALSE, FALSE
#' )
#' cards:::prop_wilson(rsp, conf_level = 0.9)
#'
#' @export
prop_wilson <- function(x, conf.level = 0.95, correct = FALSE) {
  x <- stats::na.omit(x)

  n <- length(x)
  p_hat <-

  y <-
    stats::prop.test(
      x = sum(x),
      n = length(x),
      correct = correct,
      conf.level = conf.level
    )

  list(
    N = n,
    p = mean(x),
    conf.low = y$conf.int[1],
    conf.high = y$conf.int[2],
    conf.level = conf.level,
    method =
      glue::glue("Wilson Confidence Interval {ifelse(correct, 'with', 'without')} continuity correction.")
  )
}
