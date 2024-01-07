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
#' @param strata,max.iterations arguments passed to `proportion_ci_strat_wilson()`,
#'   when `method = 'strat_wilson'`
#'
#' @return an ARD data frame
#' @export
#'
#' @examples
#' ard_proportion_ci(mtcars, variables = c(vs, am), method = "wilson")
ard_proportion_ci <- function(data, variables, by = dplyr::group_vars(data),
                              conf.level = 0.95,
                              strata,
                              max.iterations = 10,
                              method = c("waldcc", "wald", "clopper-pearson",
                                         "wilson", "wilsoncc",
                                         "strat_wilson", "strat_wilsoncc",
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


#' @describeIn proportion_ci Calculates the stratified Wilson confidence
#'   interval for unequal proportions as described in
#'   Xin YA, Su XG. Stratified Wilson and Newcombe confidence intervals
#'   for multiple binomial proportions. _Statistics in Biopharmaceutical Research_. 2010;2(3).
#'
#' @param strata (`factor`)\cr variable with one level per stratum and same length as `x`.
#' @param weights (`numeric` or `NULL`)\cr weights for each level of the strata. If `NULL`, they are
#'   estimated using the iterative algorithm that
#'   minimizes the weighted squared length of the confidence interval.
#' @param max.iterations (`count`)\cr maximum number of iterations for the iterative procedure used
#'   to find estimates of optimal weights.
#' @param correct (`flag`)\cr include the continuity correction. For further information, see for example
#'   [stats::prop.test()].
#'
#' @examples
#' # Stratified Wilson confidence interval with unequal probabilities
#'
#' set.seed(1)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' strata_data <- data.frame(
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' )
#' strata <- interaction(strata_data)
#' n_strata <- ncol(table(rsp, strata)) # Number of strata
#'
#' proportion_ci_strat_wilson(
#'   x = rsp, strata = strata,
#'   conf.level = 0.90
#' )
#'
#' # Not automatic setting of weights
#' proportion_ci_strat_wilson(
#'   x = rsp, strata = strata,
#'   weights = rep(1 / n_strata, n_strata), # TODO: I don't like using an internally calculated value here
#'   conf.level = 0.90
#' )
#'
#' @export
proportion_ci_strat_wilson <- function(x,
                                       strata,
                                       weights = NULL,
                                       conf.level = 0.95,
                                       max.iterations = 10L,
                                       correct = FALSE) {
  check_not_missing(x)
  check_not_missing(strata)
  check_class("logical", correct)
  check_class("factor", strata)
  check_range(conf.level, range = c(0, 1), scalar = TRUE)

  if (!inherits(x, "logical")) x <- as.logical(x)
  tbl <- table(factor(x, levels = c(FALSE, TRUE)), strata)
  n_strata <- length(unique(strata))

  # Checking the weights and maximum number of iterations.
  do_iter <- FALSE
  if (is.null(weights)) {
    weights <- rep(1 / n_strata, n_strata) # Initialization for iterative procedure
    do_iter <- TRUE

    # Iteration parameters
    if (!is_scalar_integerish(max.iterations) || max.iterations < 1) {
      cli::cli_abort("Argument {.arg max.iterations} must be a positive integer.")
    }
  }
  check_range(weights, range = c(0, 1), include_bounds = c(TRUE, TRUE))
  sum_weights <- sum(weights)
  if (!is_scalar_integerish(max.iterations)) {
    cli::cli_abort("Argument {.arg weights} must sum to a positive integer.")
  }
  if (as.integer(sum_weights + 0.5) != 1L) {
    cli::cli_abort("Sum of the {.arg weights} must be {.val {1L}}")
  }


  xs <- tbl["TRUE", ]
  ns <- colSums(tbl)
  use_stratum <- (ns > 0)
  ns <- ns[use_stratum]
  xs <- xs[use_stratum]
  ests <- xs / ns
  vars <- ests * (1 - ests) / ns

  strata_qnorm <- .strata_normal_quantile(vars, weights, conf.level)

  # Iterative setting of weights if they were not set externally
  weights_new <- if (do_iter) {
    .update_weights_strat_wilson(vars, strata_qnorm, weights, ns, max.iterations, conf.level)$weights
  } else {
    weights
  }

  strata_conf.level <- 2 * stats::pnorm(strata_qnorm) - 1

  ci_by_strata <- Map(
    function(x, n) {
      # Classic Wilson's confidence interval
      suppressWarnings(stats::prop.test(x, n, correct = correct, conf.level = strata_conf.level)$conf.int)
    },
    x = xs,
    n = ns
  )
  lower_by_strata <- sapply(ci_by_strata, "[", 1L)
  upper_by_strata <- sapply(ci_by_strata, "[", 2L)

  lower <- sum(weights_new * lower_by_strata)
  upper <- sum(weights_new * upper_by_strata)

  # Return values
  ret <-
    list(
      N = sum(tbl),
      conf.low = lower,
      conf.high = upper,
      conf.level = conf.level,
      weights = if (do_iter) weights_new else NULL,
      method =
        glue::glue("Stratified Wilson Confidence Interval {ifelse(correct, 'with', 'without')} continuity correction")
    ) |>
    compact()
}

#' Helper Function for the Estimation of Stratified Quantiles
#'
#' This function wraps the estimation of stratified percentiles when we assume
#' the approximation for large numbers. This is necessary only in the case
#' proportions for each strata are unequal.
#'
#' @inheritParams proportion_ci_strat_wilson
#'
#' @return Stratified quantile.
#'
#' @seealso [proportion_ci_strat_wilson()]
#'
#' @keywords internal
#' @examples
#' strata_data <- table(data.frame(
#'   "f1" = sample(c(TRUE, FALSE), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' ))
#' ns <- colSums(strata_data)
#' ests <- strata_data["TRUE", ] / ns
#' vars <- ests * (1 - ests) / ns
#' weights <- rep(1 / length(ns), length(ns))
#'
#' cards:::.strata_normal_quantile(vars, weights, 0.95)
.strata_normal_quantile <- function(vars, weights, conf.level) {
  summands <- weights^2 * vars
  # Stratified quantile
  sqrt(sum(summands)) / sum(sqrt(summands)) * stats::qnorm((1 + conf.level) / 2)
}

#' Helper Function for the Estimation of Weights for `proportion_ci_strat_wilson()`
#'
#' This function wraps the iteration procedure that allows you to estimate
#' the weights for each proportional strata. This assumes to minimize the
#' weighted squared length of the confidence interval.
#'
#' @keywords internal
#' @inheritParams proportion_ci_strat_wilson
#' @param vars (`numeric`)\cr normalized proportions for each strata.
#' @param strata_qnorm (`numeric`)\cr initial estimation with identical weights of the quantiles.
#' @param initial_weights (`numeric`)\cr initial weights used to calculate `strata_qnorm`. This can
#'   be optimized in the future if we need to estimate better initial weights.
#' @param n_per_strata (`numeric`)\cr number of elements in each strata.
#' @param max.iterations (`count`)\cr maximum number of iterations to be tried. Convergence is always checked.
#' @param tol (`number`)\cr tolerance threshold for convergence.
#'
#' @return A `list` of 3 elements: `n_it`, `weights`, and `diff_v`.
#'
#' @seealso For references and details see [`proportion_ci_strat_wilson()`].
#'
#' @examples
#' vs <- c(0.011, 0.013, 0.012, 0.014, 0.017, 0.018)
#' sq <- 0.674
#' ws <- rep(1 / length(vs), length(vs))
#' ns <- c(22, 18, 17, 17, 14, 12)
#'
#' cards:::.update_weights_strat_wilson(vs, sq, ws, ns, 100, 0.95, 0.001)
.update_weights_strat_wilson <- function(vars,
                                         strata_qnorm,
                                         initial_weights,
                                         n_per_strata,
                                         max.iterations = 50,
                                         conf.level = 0.95,
                                         tol = 0.001) {
  it <- 0
  diff_v <- NULL

  while (it < max.iterations) {
    it <- it + 1
    weights_new_t <- (1 + strata_qnorm^2 / n_per_strata)^2
    weights_new_b <- (vars + strata_qnorm^2 / (4 * n_per_strata^2))
    weights_new <- weights_new_t / weights_new_b
    weights_new <- weights_new / sum(weights_new)
    strata_qnorm <- .strata_normal_quantile(vars, weights_new, conf.level)
    diff_v <- c(diff_v, sum(abs(weights_new - initial_weights)))
    if (diff_v[length(diff_v)] < tol) break
    initial_weights <- weights_new
  }

  if (it == max.iterations) {
    warning("The heuristic to find weights did not converge with max.iterations = ", max.iterations)
  }

  list(
    "n_it" = it,
    "weights" = weights_new,
    "diff_v" = diff_v
  )
}

