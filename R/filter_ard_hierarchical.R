#' Filter Stacked Hierarchical ARDs
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to filter stacked hierarchical ARDs.
#'
#' For the purposes of this function, we define a "variable group" as a combination of ARD rows grouped by the
#' combination of all their variable levels, but excluding any `by` variables.
#'
#' @param x (`card`)\cr
#'   a stacked hierarchical ARD of class `'card'` created using [`ard_stack_hierarchical()`].
#' @param filter (`expression`)\cr
#'   an expression that is used to filter variable groups of the hierarchical ARD. See the Details section below.
#'
#' @details
#' The `filter` argument can be used to filter out variable groups of a hierarchical ARD which do not meet the
#' requirements provided as an expression. Variable groups can be filtered on the values of any of the possible
#' statistics (`n`, `p`, and `N`) provided they are included at least once in the ARD, as well as the values of any
#' `by` variables. For each variable group that does not meet the filtering requirement, all statistics (rows)
#' corresponding to that group will be removed from the ARD. In addition to filtering on individual statistic values,
#' filters can be applied across the variable group (i.e. across all `by` variable values) by using aggregate functions
#' such as `sum()` and `mean()`. Filtering is only applied to variable groups that correspond to the innermost variable
#' in the hierarchy -- all outer (summary) variable groups will be kept.
#'
#' For example, consider an ARD created using [ard_stack_hierarchical()] with `variables = c(AESOC, AEDECOD)` and
#' `by = ARM` to summarize adverse events (AEs) in each system organ class (SOC) by treatment arm. Each
#' "variable group" will be comprised of all rows corresponding to a unique SOC/AE combination in any treatment arm. If
#' a filter of `n > 3` is applied, each variable group will be checked for at least one row that corresponds to the
#' `n` statistic and has a value greater than 3, and if any row satisfies the filter condition then all rows from the
#' variable group (i.e. all statistics & treatment groups) will be retained in the ARD. Any overall or "outer" variable
#' groups that count _all_ AEs corresponding to a unique SOC are kept whether or not the filter condition is met within
#' the variable group.
#'
#' If data for overall statistics is present in the ARD (`ard_stack_hierarchical(overall=TRUE)`), this data will not be
#' used in any filters (i.e. `sum(n)` will not include the overall `n` in a given group). To filter on overall
#' statistics use the `sum()` function in your filter instead (i.e. `sum(n)` is equal to the "overall" `n` across any
#' `by` variables).
#'
#' If no `by` variables were used (i.e. `ard_stack_hierarchical(by=NULL)`), the only grouping performed prior to
#' filtering will be grouping all statistics together for each combination of variable levels, i.e. filters will be
#' applied independently to each combination of variable levels. Using the previous example of an ARD of AEs by SOC,
#' if `by=NULL` then each "variable group" will contain all rows corresponding to a unique SOC/AE combination.
#'
#' Some examples of possible filters:
#' - `filter = n > 5` - keep AEs from variable groups where at least one level in the group has more than 3 AEs observed
#' - `filter = n == 2 & p < 0.05` - keep AEs from variable groups where at least one level in the group has exactly 2
#'   AEs observed _and_ at least one level in the variable group has a prevalence of less than 5%
#' - `filter = sum(n) >= 4` - keep AEs from variable groups where at least 4 AEs are observed across the group
#' - `filter = mean(n) > 4 | n > 3` - keep AEs from variable groups where an average of least 4 AEs is observed across
#'   the levels of the group _or_ at least one level in the variable group has more than 3 AEs observed
#' - `filter = any(n > 2 & TRTA == "Xanomeline High Dose")` - keep AEs from variable groups where more than 2 AEs
#'   are observed in a record from the variable group where the `by` variable is `TRTA` and the level of `TRTA` is
#'   `"Xanomeline High Dose"`.
#'
#' @return an ARD data frame of class 'card'
#' @seealso [sort_ard_hierarchical()]
#' @name filter_ard_hierarchical
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' # create a base AE ARD
#' ard <- ard_stack_hierarchical(
#'   ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM),
#'   id = USUBJID
#' )
#'
#' # Example 1 ----------------------------------
#' # Keep AEs from TRTA groups where more than 3 AEs are observed across the group
#' filter_ard_hierarchical(ard, sum(n) > 3)
#'
#' # Example 2 ----------------------------------
#' # Keep AEs where at least one level in the TRTA group has more than 3 AEs observed
#' filter_ard_hierarchical(ard, n > 3)
#'
#' # Example 3 ----------------------------------
#' # Keep AEs that have an overall prevalence of greater than 5%
#' filter_ard_hierarchical(ard, sum(n) / sum(N) > 0.05)
NULL

#' @rdname filter_ard_hierarchical
#' @export
filter_ard_hierarchical <- function(x, filter) {
  set_cli_abort_call()

  # check and process inputs ---------------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(filter)
  check_class(x, "card")
  if (!"args" %in% names(attributes(x))) {
    cli::cli_abort(
      "Filtering is only available for stacked hierarchical ARDs created using {.fun ard_stack_hierarchical}.",
      call = get_cli_abort_call()
    )
  }
  filter <- enquo(filter)
  if (!quo_is_call(filter)) {
    cli::cli_abort(
      "{.arg filter} must be an expression.",
      call = get_cli_abort_call()
    )
  }

  by <- attributes(x)$args$by
  by_cols <- if (length(by) > 0) paste0("group", seq_along(by), c("", "_level")) else NULL
  if (!all(all.vars(filter) %in% c(x$stat_name, by))) {
    var_miss <- setdiff(all.vars(filter), c(x$stat_name, by))
    cli::cli_abort(
      paste(
        "The expression provided as {.arg filter} includes condition{?s} for statistic{?s} or `by` variable{?s}",
        "{.val {var_miss}} which {?is/are} not present in the ARD."
      ),
      call = get_cli_abort_call()
    )
  }

  # ignore "overall" data
  is_overall <- apply(x, 1, function(x) !isTRUE(any(x %in% by)))
  if (length(by) > 0  && sum(is_overall) > 0) {
    x <- x[!is_overall, ]
  }

  # reshape ARD so each stat is in its own column ------------------------------------------------
  x_f <- x |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    dplyr::select(all_ard_groups(), all_ard_variables(), "stat_name", "stat", "idx") |>
    tidyr::pivot_wider(
      id_cols = c(all_ard_groups(), all_ard_variables()),
      names_from = "stat_name",
      values_from = "stat",
      values_fn = unlist,
      unused_fn = list
    )

  # apply filter ---------------------------------------------------------------------------------
  f_idx <- x_f |>
    dplyr::group_by(across(c(all_ard_groups(), all_ard_variables(), -all_of(by_cols)))) |>
    dplyr::group_map(\(.df, .g) {
      # allow filtering on `by` variable levels
      if (length(by) > 0) names(.df)[names(.df) == by_cols[c(FALSE, TRUE)]] <- by

      # only filter rows for innermost variable
      if (.g$variable == dplyr::last(attributes(x)$args$variables)) {
        .df[["idx"]][eval_tidy(filter, data = .df)]
      } else {
        .df[["idx"]]
      }
    }) |>
    unlist() |>
    sort()

  x[f_idx, ]
}
