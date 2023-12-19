#' Defaults for statistical arguments
#'
#' @description
#' -`default_stat_labels()` returns a named list of statistics labels
#'
#' - `default_fmt_fns()` returns a named list of formatting functions to be applied in lieu
#' of the global default, which is to round to one decimal place.
#'
#' @return named list of values
#'
#' @examples
#' # stat labels
#' default_stat_labels()
#'
#' # formatting functions
#' default_fmt_fns()
#'
#' @rdname default_stat_args
#' @export
default_stat_labels <- function() {

  list(
    mean = "Mean",
    sd = "SD",
    var = "Variance",
    median = "Median",
    p25 = "25th Percentile",
    p75 = "75th Percentile",
    min = "Min",
    max = "Max",

    n = "n",
    N = "N",
    length = "Vector Length",
    p = "%",
    p_cell = "%",

    N_obs = "Vector Length",
    N_miss = "N Missing",
    N_nonmiss = "N Non-missing",
    p_miss = "% Missing",
    p_nonmiss = "% Non-missing"
  )
}

#' @rdname default_stat_args
#' @export
default_fmt_fns <- function(){

  list(
    n = function(x) format(round5(x, digits = 0), nsmall = 0),
    N = function(x) format(round5(x, digits = 0), nsmall = 0),
    N_obs = function(x) format(round5(x, digits = 0), nsmall = 0),
    N_miss = function(x) format(round5(x, digits = 0), nsmall = 0),
    N_nonmiss = function(x) format(round5(x, digits = 0), nsmall = 0),
    p = function(x) format(round5(x * 100, digits = 1), nsmall = 1),
    p_cell = function(x) format(round5(x * 100, digits = 1), nsmall = 1),
    p_miss = function(x) format(round5(x * 100, digits = 1), nsmall = 1),
    p_nonmiss = function(x) format(round5(x * 100, digits = 1), nsmall = 1)
  )

}
