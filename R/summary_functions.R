#' Summary Functions
#'
#' @description
#' - `continuous_variable_summary_fns()` returns a named list of summary functions
#'   for continuous variables. Some functions include slight modifications to
#'   their base equivalents. For example, the `min()` and `max()` functions
#'   return `NA` instead of `Inf` when an empty vector is passed.
#'
#' - `categorical_variable_summary_fns()` summary functions for categorical
#'   variables.
#'
#' - `missing_variable_summary_fns()` summary functions suitable for variable-level
#'   summaries, such as number and rate of missing data.
#'
#' @param summaries (`character`)\cr
#'   a character vector of results to include in output.
#'
#'   - `categorical_variable_summary_fns()`: Select one or more from 'N', 'n', 'p'
#'
#'   - `continuous_variable_summary_fns()`: Select one or more from
#'     `r formals(continuous_variable_summary_fns)[["summaries"]] |> eval()  %>% {paste(shQuote(.), collapse = ", ")}`.
#'
#'   - `missing_variable_summary_fns()`: Select one or more from
#'     `r formals(missing_variable_summary_fns)[["summaries"]] |> eval()  %>% {paste(shQuote(.), collapse = ", ")}`.
#'
#' @return named list of summary functions
#' @name summary_functions
#'
#' @examples
#' # continuous variable summaries
#' ard_continuous(
#'   ADSL,
#'   variables = "AGE",
#'   statistics = ~continuous_variable_summary_fns(c("N", "median"))
#' ) |>
#'   flatten_ard()
#'
#' # categorical variable summaries
#' ard_categorical(
#'   ADSL,
#'   variables = "AGEGR1",
#'   statistics = ~categorical_variable_summary_fns(c("n", "N"))
#' ) |>
#'   flatten_ard()
#'
#' # summary for rates of missing data
#' ard_missing(
#'   ADSL,
#'   variables = c("AGE", "AGEGR1"),
#'   statistics = ~missing_variable_summary_fns()
#' ) |>
#'   flatten_ard()
NULL

#' @rdname summary_functions
#' @export
categorical_variable_summary_fns <- function(summaries = c("n", "p", "N")) {
  summaries <- rlang::arg_match(summaries, multiple = TRUE)

  tabulate_fn <-
    function(x, stats = summaries) {

      res <- list()
      if (any(c("N", "p") %in% stats))
        res[["N"]] <- length(x)
      if (any(c("n", "p") %in% stats))
        res[["n"]] <- table(x)
      if ("p" %in% stats)
        res$p <- res$n / res$N

      res
    }

  list(tabulate = tabulate_fn)
}

#' @rdname summary_functions
#' @export
continuous_variable_summary_fns <- function(summaries = c('N', 'mean', 'sd', 'median',
                                                          'p25', 'p75', 'min', 'max')) {
  # process the selection of the summary stats to include ----------------------
  summaries <- rlang::arg_match(summaries, multiple = TRUE)

  # list all functions available by default ------------------------------------
  list_fns <-
    list(
      N = function(x) length(x),
      mean = function(x) mean(x, na.rm = TRUE),
      sd = function(x) stats::sd(x, na.rm = TRUE),
      median = function(x) stats::median(x, na.rm = TRUE),
      p25 = function(x) stats::quantile(x, probs = 0.25, na.rm = TRUE) |> unname(),
      p75 = function(x) stats::quantile(x, probs = 0.75, na.rm = TRUE) |> unname(),
      min = function(x) ifelse(length(x) == 0L, .classed_NA(x), min(x, na.rm = TRUE)),
      max = function(x) ifelse(length(x) == 0L, .classed_NA(x), max(x, na.rm = TRUE))
    )

  # return list of functions ---------------------------------------------------
  list_fns[summaries]
}

#' @rdname summary_functions
#' @export
missing_variable_summary_fns <- function(summaries = c("N_obs", "N_miss" , "N_nonmiss", "p_miss", "p_nonmiss")) {
  summaries <- rlang::arg_match(summaries, multiple = TRUE)

  list(
    var_level =
      function(x, stats = summaries) {
        res <- list()

        if (any(c("N_obs", "N_nonmiss", "p_miss", "p_nonmiss") %in% stats))
          res[["N_obs"]] <- length(x)
        if (any(c("N_miss", "N_nonmiss", "p_miss") %in% stats))
          res[["N_miss"]] <- sum(!x)
        if (any(c("N_nonmiss", "p_nonmiss") %in% stats))
          res[["N_nonmiss"]] <- res[["N_obs"]] - res[["N_miss"]]
        if ("p_miss" %in% stats)
          res[["p_miss"]] <- res[["N_miss"]] / res[["N_obs"]]
        if ("p_nonmiss" %in% stats)
          res[["p_nonmiss"]] <- res[["N_nonmiss"]] / res[["N_obs"]]

        res
      }
  )
}



# returns NA the same class as the input
.classed_NA <- function(x, length = 1L) {
  if (inherits(x, "character")) return(rep_len(NA_character_, length.out = length))
  if (inherits(x, "integer")) return(rep_len(NA_integer_, length.out = length))
  if (inherits(x, "numeric")) return(rep_len(NA_real_, length.out = length))

  rep_len(NA, length.out = length)
}
