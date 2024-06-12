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
#' @param summaries (`character`)\cr
#'   a character vector of results to include in output. Select one or more from
#'     `r eval(formals(continuous_summary_fns)$summaries) %>% {paste(shQuote(., "sh"), collapse = ", ")}`.
#' @param other_stats (named `list`)\cr
#'   named list of other statistic functions to supplement the pre-programmed functions.
#'
#' @return named list of summary statistics
#' @name summary_functions
#'
#' @examples
#' # continuous variable summaries
#' ard_continuous(
#'   ADSL,
#'   variables = "AGE",
#'   statistic = ~ continuous_summary_fns(c("N", "median"))
#' )
NULL

#' @rdname summary_functions
#' @export
continuous_summary_fns <- function(summaries = c(
                                     "N", "mean", "sd", "median",
                                     "p25", "p75", "min", "max"
                                   ),
                                   other_stats = NULL) {
  set_cli_abort_call()

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
