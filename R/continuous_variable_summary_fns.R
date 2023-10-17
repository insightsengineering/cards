#' List of Continuous Summary Functions
#'
#' Returns a list of univariate summary functions.
#' Some functions include slight modifications to their base equivalents. For
#' example, the `min()` and `max()` functions return `NA` instead of `Inf`
#' when an empty vector is passed.
#'
#' @param summaries a character vector of functions to include in output. Default
#' is `NULL`, which selects all summary functions. Select one or more from
#' `r continuous_variable_summary_fns() |> names() %>% {paste(shQuote(.), collapse = ", ")}`.
#'
#' @return named list of summary functions
#' @export
#'
#' @examples
#' continuous_variable_summary_fns(c("N", "median"))
continuous_variable_summary_fns <- function(summaries = NULL) {
  # list all functions available by default ------------------------------------
  list_fns <-
    list(
      N = function(x) length(x),
      # N_miss = function(x) sum(is.na(x)),
      # p_miss = function(x) sum(is.na(x)) / length(x),
      # length = function(x) length(x),
      mean = function(x) mean(x, na.rm = TRUE),
      sd = function(x) stats::sd(x, na.rm = TRUE),
      median = function(x) stats::median(x, na.rm = TRUE),
      p25 = function(x) stats::quantile(x, probs = 0.25, na.rm = TRUE) |> unname(),
      p75 = function(x) stats::quantile(x, probs = 0.75, na.rm = TRUE) |> unname(),
      min = function(x) ifelse(length(x) == 0L, .classed_NA(x), min(x, na.rm = TRUE)),
      max = function(x) ifelse(length(x) == 0L, .classed_NA(x), max(x, na.rm = TRUE))
    )

  # process the selection of the summary stats to include ----------------------
  if (is.null(summaries)) summaries <- names(list_fns)
  summaries <- match.arg(summaries, choices = names(list_fns), several.ok = TRUE)

  # return list of functions ---------------------------------------------------
  list_fns[summaries]
}


# returns NA the same class as the input
.classed_NA <- function(x, length = 1L) {
  if (inherits(x, "character")) return(rep_len(NA_character_, length.out = length))
  if (inherits(x, "integer")) return(rep_len(NA_integer_, length.out = length))
  if (inherits(x, "numeric")) return(rep_len(NA_real_, length.out = length))

  rep_len(NA, length.out = length)
}
