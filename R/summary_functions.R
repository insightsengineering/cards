#' Summary Functions
#'
#' @description
#' - `continuous_summary_fns()` returns a named list of summary functions
#'   for continuous variables. Some functions include slight modifications to
#'   their base equivalents. For example, the `min()` and `max()` functions
#'   return `NA` instead of `Inf` when an empty vector is passed.
#'   Statistics `"p25"` and `"p75"` are calculated with `quantile(type = 2)`,
#'   which matches
#'   [SAS's default value](https://psiaims.github.io/CAMIS/Comp/r-sas-summary-stats.html).
#'
#' - `categorical_summary_fns()` returns a named list of summary statistics for
#'   categorical variables. Options are `"n"`, `"N"`, and `"p"`. If a user requests, for
#'   example, only `"p"`, the function will return `"n"` and `"N"` as well, since they are
#'   needed to calculate `"p"`. These statistics will be stored as a vector within the
#'   `tabulation` list element.
#'
#' - `missing_summary_fns()` returns a named list of summary functions suitable
#'   for variable-level summaries, such as number and rate of missing data.
#'
#' @param summaries (`character`)\cr
#'   a character vector of results to include in output.
#'
#'   - `continuous_summary_fns()`: Select one or more from
#'     `r eval(formals(continuous_summary_fns)$summaries) %>% {paste(shQuote(., "sh"), collapse = ", ")}`.
#'
#'   - `categorical_summary_fns()`: Select one or more from
#'     `r eval(formals(categorical_summary_fns)$summaries) %>% {paste(shQuote(., "sh"), collapse = ", ")}`.
#'
#'   - `missing_summary_fns()`: Select one or more from
#'     `r eval(formals(missing_summary_fns)$summaries) %>% {paste(shQuote(., "sh"), collapse = ", ")}`.
#' @param other_stats (named `list`)\cr
#'   named list of other statistic functions to supplement the pre-programmed functions.
#'
#' @return `continuous_summary_fns()` and `missing_summary_fns()` return a named list of summary
#' functions, `categorical_summary_fns()` returns a named list of summary statistics.
#' @name summary_functions
#'
#' @examples
#' # continuous variable summaries
#' ard_continuous(
#'   ADSL,
#'   variables = "AGE",
#'   statistic = ~ continuous_summary_fns(c("N", "median"))
#' )
#'
#' # categorical variable summaries
#' ard_categorical(
#'   ADSL,
#'   variables = "AGEGR1",
#'   statistic = ~ categorical_summary_fns(c("n", "N"))
#' )
#'
#' # summary for rates of missing data
#' ard_missing(
#'   ADSL,
#'   variables = c("AGE", "AGEGR1"),
#'   statistic = ~ missing_summary_fns()
#' )
NULL

#' @rdname summary_functions
#' @export
continuous_summary_fns <- function(summaries = c(
                                     "N", "mean", "sd", "median",
                                     "p25", "p75", "min", "max"
                                   ),
                                   other_stats = NULL) {
  # process the selection of the summary stats to include ----------------------
  summaries <- arg_match(summaries, multiple = TRUE)

  # list all functions available by default ------------------------------------
  list_fns <-
    list(
      N = function(x) length(x),
      mean = function(x) mean(x, na.rm = TRUE),
      sd = function(x) stats::sd(x, na.rm = TRUE),
      median = function(x) stats::median(x, na.rm = TRUE),
      p25 = function(x) stats::quantile(x, probs = 0.25, na.rm = TRUE, type = 2) |> unname(),
      p75 = function(x) stats::quantile(x, probs = 0.75, na.rm = TRUE, type = 2) |> unname(),
      min = function(x) {
        if (length(x) == 0L) {
          return(structure(NA, class = class(x)))
        }
        min(x, na.rm = TRUE)
      },
      max = function(x) {
        if (length(x) == 0L) {
          return(structure(NA, class = class(x)))
        }
        max(x, na.rm = TRUE)
      }
    )

  # return list of functions ---------------------------------------------------
  list_fns[summaries] |>
    c(other_stats)
}

#' @rdname summary_functions
#' @export
categorical_summary_fns <- function(summaries = c("n", "p", "N"), other_stats = NULL) {
  # check inputs ---------------------------------------------------------------
  if (!is_empty(summaries)) {
    summaries <-
      arg_match(summaries, values = c("n", "p", "N"), multiple = TRUE)
  }
  check_class(x = other_stats, cls = "list", allow_empty = TRUE)

  # combine tabulation and other stats -----------------------------------------
  lst_all_stats <-
    list(tabulation = summaries) |>
    c(other_stats)

  lst_all_stats
}

#' @rdname summary_functions
#' @export
missing_summary_fns <- function(summaries = c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss")) {
  summaries <- arg_match(summaries, multiple = TRUE)

  list(
    var_level =
      function(x, stats = summaries) {
        res <- list()

        if (any(c("N_obs", "N_nonmiss", "p_miss", "p_nonmiss") %in% stats)) {
          res[["N_obs"]] <- length(x)
        }
        if (any(c("N_miss", "N_nonmiss", "p_miss") %in% stats)) {
          res[["N_miss"]] <- sum(!x)
        }
        if (any(c("N_nonmiss", "p_nonmiss") %in% stats)) {
          res[["N_nonmiss"]] <- res[["N_obs"]] - res[["N_miss"]]
        }
        if ("p_miss" %in% stats) {
          res[["p_miss"]] <- res[["N_miss"]] / res[["N_obs"]]
        }
        if ("p_nonmiss" %in% stats) {
          res[["p_nonmiss"]] <- res[["N_nonmiss"]] / res[["N_obs"]]
        }

        res
      }
  )
}
