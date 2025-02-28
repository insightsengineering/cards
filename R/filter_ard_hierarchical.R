#' Filter Stacked Hierarchical ARDs
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to filter stacked hierarchical ARDs.
#'
#' @param x (`card`)\cr
#'   a stacked hierarchical ARD of class `'card'` created using [`ard_stack_hierarchical()`].
#' @param filter (`expression`)\cr
#'   an expression that is used to filter rows of the hierarchical ARD. See the Details
#'   section below.
#'
#' @details
#' The `filter` argument can be used to filter out rows of a hierarchical ARD which do not meet the requirements
#' provided as an expression. Rows can be filtered on the values of any of the possible statistics (`n`, `p`, and `N`)
#' provided they are included at least once in the ARD, as well as the values of any `by` variables. For each entry that
#' does not meet the filtering requirement, all statistics corresponding to that entry will be removed from the ARD.
#' Filtering is only applied to rows that correspond to the innermost variable in the hierarchy---all outer variable
#' (summary) rows will be kept. In addition to filtering on individual statistic values, filters can be applied across
#' the hierarchical row (i.e. across all `by` variable values) by using aggregate functions such as `sum()` and
#' `mean()`.
#'
#' Some examples of possible filters:
#' - `filter = n > 5`
#' - `filter = n == 2 & p < 0.05`
#' - `filter = sum(n) >= 4`
#' - `filter = mean(n) > 4 | n > 3`
#' - `filter = any(n > 2 & TRTA == "Xanomeline High Dose")`
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
#' # Keep AEs that have more than 3 observed across the TRTA groups
#' filter_ard_hierarchical(ard, sum(n) > 3)
#'
#' # Example 2 ----------------------------------
#' # Keep AEs where at least one TRTA group has more than 3 AEs observed
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
  if (!all(all.vars(filter) %in% c(x$stat_name, attributes(x)$args$by))) {
    var_miss <- setdiff(all.vars(filter), c(x$stat_name, attributes(x)$args$by))
    cli::cli_abort(
      paste(
        "The expression provided as {.arg filter} includes condition{?s} for statistic{?s} or `by` variable{?s}",
        "{.val {var_miss}} which {?is/are} not present in the ARD."
      ),
      call = get_cli_abort_call()
    )
  }

  by_cols <- paste0("group", seq_along(length(attributes(x)$args$by)), c("", "_level"))

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
      names(.df)[names(.df) == by_cols[c(FALSE, TRUE)]] <- attributes(x)$args$by

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
