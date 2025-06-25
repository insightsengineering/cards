#' Update ARDs
#'
#' @description
#' Functions used to update ARD formatting functions and statistic labels.
#'
#' This is a helper function to streamline the update process. If it does not
#' exactly meet your needs, recall that an ARD is just a data frame and it
#' can be modified directly.
#'
#' @param x (`data.frame`)\cr
#'  an ARD data frame of class 'card'
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables in `x$variable` to apply update. Default is `everything()`.
#' @param stat_names (`character`)\cr
#'   character vector of the statistic names (i.e. values from `x$stat_name`) to
#'   apply the update.
#' @param fmt_fun (`function`)\cr
#'   a function or alias recognized by `alias_as_fmt_fun()`.
#' @param stat_label (`function`)\cr
#'   a string of the updated statistic label.
#' @param filter (`expression`)\cr
#'   an expression that evaluates to a logical vector identifying rows in `x`
#'   to apply the update to. Default is `TRUE`, and update is applied to
#'   all rows.
#' @param fmt_fn `r lifecycle::badge("deprecated")`
#'
#' @return an ARD data frame of class 'card'
#' @name update_ard
#'
#' @examples
#' ard_continuous(ADSL, variables = AGE) |>
#'   update_ard_fmt_fun(stat_names = c("mean", "sd"), fmt_fun = 8L) |>
#'   update_ard_stat_label(stat_names = c("mean", "sd"), stat_label = "Mean (SD)") |>
#'   apply_fmt_fun()
#'
#' # same as above, but only apply update to the Placebo level
#' ard_continuous(
#'   ADSL,
#'   by = ARM,
#'   variables = AGE,
#'   statistic = ~ continuous_summary_fns(c("N", "mean"))
#' ) |>
#'   update_ard_fmt_fun(stat_names = "mean", fmt_fun = 8L, filter = group1_level == "Placebo") |>
#'   apply_fmt_fun()
NULL

#' @export
#' @rdname update_ard
update_ard_fmt_fun <- function(x, variables = everything(), stat_names, fmt_fun, filter = TRUE, fmt_fn = deprecated()) {
  set_cli_abort_call()

  # deprecated args ------------------------------------------------------------
  if (lifecycle::is_present(fmt_fn)) {
    lifecycle::deprecate_soft(
      when = "0.6.1",
      what = "update_ard_fmt_fun(fmt_fn)",
      with = "update_ard_fmt_fun(fmt_fun)"
    )
    fmt_fun <- fmt_fn
  }

  # check and process inputs ---------------------------------------------------
  check_class(x, "card")
  process_selectors(data = dplyr::tibble(!!!rep_named(unique(x$variable), NA)), variables = {{ variables }})
  check_class(stat_names, "character")
  check_length(fmt_fun, 1L)

  # construct lgl index condition ----------------------------------------------
  # first evaluate the variable and stat_names
  idx1 <-
    eval_tidy(expr(.data$variable %in% variables & .data$stat_name %in% stat_names), data = x)

  # and then add any additional reqs passed in `filter`
  idx2 <-
    tryCatch(
      eval_tidy(enquo(filter), data = x),
      error = function(e) {
        cli::cli_abort(
          c("There was an error evaluating the {.arg filter} argument. See below:",
            "x" = "{conditionMessage(e)}"
          ),
          call = get_cli_abort_call()
        )
      }
    )
  if (!is.vector(idx2) || !is.logical(idx2) || (length(idx2) != 1L && length(idx2) != nrow(x))) {
    cli::cli_abort(
      "The {.arg filter} argument must be an expression that evaluates to a
       {.cls logical} vector of length {.val {1L}} or {.val {nrow(x)}}.",
      call = get_cli_abort_call()
    )
  }

  # update ARD with new fmt_fun ------------------------------------------------
  x$fmt_fun[idx1 & idx2] <- list(alias_as_fmt_fun(fmt_fun))

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
  # first evaluate the variable and stat_names
  idx1 <-
    eval_tidy(expr(.data$variable %in% variables & .data$stat_name %in% stat_names), data = x)

  # and then add any additional reqs passed in `filter`
  idx2 <-
    tryCatch(
      eval_tidy(enquo(filter), data = x),
      error = function(e) {
        cli::cli_abort(
          c("There was an error evaluating the {.arg filter} argument. See below:",
            "x" = "{conditionMessage(e)}"
          ),
          call = get_cli_abort_call()
        )
      }
    )
  if (!is.vector(idx2) || !is.logical(idx2) || (length(idx2) != 1L && length(idx2) != nrow(x))) {
    cli::cli_abort(
      "The {.arg filter} argument must be an expression that evaluates to a
       {.cls logical} vector of length {.val {1L}} or {.val {nrow(x)}}.",
      call = get_cli_abort_call()
    )
  }

  # update ARD with new stat_label ---------------------------------------------
  x$stat_label[idx1 & idx2] <- stat_label

  # return ard -----------------------------------------------------------------
  x
}
