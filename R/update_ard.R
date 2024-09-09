#' Update ARDs
#'
#' Functions used to update ARD formatting functions and statistic labels.
#'
#' @param x (`data.frame`)\cr
#'  an ARD data frame of class 'card'
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables in `x$variable` to apply update. Default is `everything()`.
#' @param stat_names (`character`)\cr
#'   character vector of the statistic names (i.e. values from `x$stat_name`) to
#'   apply the update.
#' @param fmt_fn (`function`)\cr
#'   a function or alias recognized by `alias_as_fmt_fn()`.
#' @param stat_label (`function`)\cr
#'   a string of the updated statistic label.
#'
#' @return an ARD data frame of class 'card'
#' @name update_ard
#'
#' @examples
#' ard_continuous(ADSL, variables = AGE) |>
#'   update_ard_fmt_fn(stat_names = c("mean", "sd"), fmt_fn = 8L) |>
#'   update_ard_stat_label(stat_names = c("mean", "sd"), stat_label = "Mean (SD)") |>
#'   apply_fmt_fn()
NULL

#' @export
#' @rdname update_ard
update_ard_fmt_fn <- function(x, variables = everything(), stat_names, fmt_fn) {
  set_cli_abort_call()

  # check and process inputs ---------------------------------------------------
  check_class(x, "card")
  process_selectors(data = dplyr::tibble(!!!rep_named(unique(x$variable), NA)), variables = {{ variables }})
  check_class(stat_names, "character")
  check_length(fmt_fn, 1L)

  # update ARD with new fmt_fn -------------------------------------------------
  x$fmt_fn[x$variable %in% variables & x$stat_name %in% stat_names] <-
    list(alias_as_fmt_fn(fmt_fn))

  # return ard -----------------------------------------------------------------
  x
}

#' @export
#' @rdname update_ard
update_ard_stat_label <- function(x, variables = everything(), stat_names, stat_label) {
  # check and process inputs ---------------------------------------------------
  check_class(x, "card")
  process_selectors(data = dplyr::tibble(!!!rep_named(unique(x$variable), NA)), variables = {{ variables }})
  check_class(stat_names, "character")
  check_string(stat_label)

  # update ARD with new fmt_fn -------------------------------------------------
  x$stat_label[x$variable %in% variables & x$stat_name %in% stat_names] <- stat_label

  # return ard -----------------------------------------------------------------
  x
}
