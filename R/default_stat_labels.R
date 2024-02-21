#' Defaults for Statistical Arguments
#'
#' Returns a named list of statistics labels
#'
#' @return named list
#' @export
#'
#' @examples
#' # stat labels
#' default_stat_labels()
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
    p = "%",
    N_obs = "Vector Length",
    N_miss = "N Missing",
    N_nonmiss = "N Non-missing",
    p_miss = "% Missing",
    p_nonmiss = "% Non-missing"
  )
}
