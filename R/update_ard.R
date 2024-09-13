#' Update ARDs
#'
#' @description
#' Functions used to update ARD formatting functions and statistic labels.
#'
#' This is a helper function to streamline the update process. If it does not
#' exaclty meet your needs, recall that an ARD is just a data frame and it
#' can be modified directly.
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
#' @param filter (`expression`)\cr
#'   an expression that evaluates to a logical vector identifying rows in `x`
#'   to apply the update to. Default is `TRUE`, and update is applied to
#'   all rows.
#'
#' @return an ARD data frame of class 'card'
#' @name update_ard
#'
#' @examples
#' ard_continuous(ADSL, variables = AGE) |>
#'   update_ard_fmt_fn(stat_names = c("mean", "sd"), fmt_fn = 8L) |>
#'   update_ard_stat_label(stat_names = c("mean", "sd"), stat_label = "Mean (SD)") |>
#'   apply_fmt_fn()
#'
#' # same as above, but only apply update to the Placebo level
#' ard_continuous(
#'   ADSL,
#'   by = ARM,
#'   variables = AGE,
#'   statistic = ~ continuous_summary_fns(c("N", "mean"))
#' ) |>
#'   update_ard_fmt_fn(stat_names = "mean", fmt_fn = 8L, filter = group1_level == "Placebo") |>
#'   apply_fmt_fn()
NULL

#' @export
#' @rdname update_ard
update_ard_fmt_fn <- function(x, variables = everything(), stat_names, fmt_fn, filter = TRUE) {
  set_cli_abort_call()

  # check and process inputs ---------------------------------------------------
  check_class(x, "card")
  process_selectors(data = dplyr::tibble(!!!rep_named(unique(x$variable), NA)), variables = {{ variables }})
  check_class(stat_names, "character")
  check_length(fmt_fn, 1L)

  # construct lgl index condition ----------------------------------------------
  idx <-
    # first evaluate the variable and stat_names
    eval_tidy(expr(.data$variable %in% variables & .data$stat_name %in% stat_names), data = x) &
      # and then add any additional reqs passed in `filter`
      eval_tidy(enquo(filter), data = x)

  # update ARD with new fmt_fn -------------------------------------------------
  x$fmt_fn[idx] <- list(alias_as_fmt_fn(fmt_fn))

  # return ard -----------------------------------------------------------------
  x
}

#' @export
#' @rdname update_ard
update_ard_stat_label <- function(x, variables = everything(), stat_names, stat_label, filter = TRUE) {
  # check and process inputs ---------------------------------------------------
  check_class(x, "card")
  process_selectors(data = dplyr::tibble(!!!rep_named(unique(x$variable), NA)), variables = {{ variables }})
  check_class(stat_names, "character")
  check_string(stat_label)

  # construct lgl index condition ----------------------------------------------
  idx <-
    # first evaluate the variable and stat_names
    eval_tidy(expr(.data$variable %in% variables & .data$stat_name %in% stat_names), data = x) &
      # and then add any additional reqs passed in `filter`
      eval_tidy(enquo(filter), data = x)

  # update ARD with new stat_label ---------------------------------------------
  x$stat_label[idx] <- stat_label

  # return ard -----------------------------------------------------------------
  x
}
