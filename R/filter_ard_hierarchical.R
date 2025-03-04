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
#' The `filter` argument can be used to filter out variable groups of a hierarchical
#'   ARD which do not meet the requirements provided as an expression.
#' Variable groups can be filtered on the values of any of the possible
#'   statistics (`n`, `p`, and `N`) provided they are included at least once
#'   in the ARD, as well as the values of any `by` variables.
#'
#' To illustrate how the function works, consider the typical example below
#'   where the AE summaries are provided by treatment group.
#'
#' ```r
#' ADAE |>
#'   dplyr::filter(AESOC == "GASTROINTESTINAL DISORDERS",
#'                 AEDECOD %in% c("VOMITING", "DIARRHOEA")) |>
#'   ard_stack_hierarchical(
#'     variables = c(AESOC, AEDECOD),
#'     by = TRTA,
#'     denominator = ADSL |> dplyr::rename(TRTA = ARM),
#'     id = USUBJID
#'   )
#' ```
#'
#' |**SOC** / AE                   |  Placebo  | Xanomeline High Dose  | Xanomeline Low Dose  |
#' |:------------------------------|----------:|----------------------:|---------------------:|
#' |__GASTROINTESTINAL DISORDERS__ | 11 (13%)  |             10 (12%)  |            8 (9.5%)  |
#' |DIARRHOEA                      |  9 (10%)  |             4 (4.8%)  |            5 (6.0%)  |
#' |VOMITING                       | 3 (3.5%)  |             7 (8.3%)  |            3 (3.6%)  |
#'
#' Filters applied to the summary statistics of the innermost variable in the hierarchies---`AEDECOD`
#'   in this case.
#' If any of the summary statistics meet the filter requirement for any of the treatment groups,
#'   the entire row is retained.
#' For example, if `filter = n >= 9` were passed, the criteria would be met for DIARRHOEA
#'   as the Placebo group observed 9 AEs and as a result the summary statistics for the other
#'   treatment groups are retained as well.
#' Conversely, no treatment groups' summary statistics satisfy the filter requirement
#'   for VOMITING and all rows associated with this AE will be removed.
#'
#' In addition to filtering on individual statistic values, filters can be applied
#'   across the treatment groups (i.e. across all `by` variable values) by using
#'   aggregate functions such as `sum()` and `mean()`.
#' A value of `filter = sum(n) >= 18` retains AEs where the sum of the number
#'   of AEs across the treatment groups is greater than or equal to 18.
#'
#' If `ard_stack_hierarchical(overall=TRUE)` was run, the overall column is
#'   __not__ considered in any filtering.
#'
#' Some examples of possible filters:
#' - `filter = n > 5`: keep AEs where one of the treatment groups observed more than 5 AEs
#' - `filter = n == 2 & p < 0.05`: keep AEs where one of the treatment groups observed exactly 2
#'    AEs observed _and_ the proportion is less than 5%
#' - `filter = sum(n) >= 4`: keep AEs where there were 4 or more observed across the treatment groups
#' - `filter = mean(n) > 4 | n > 3`: keep AEs where there were mean number of AEs is 4 or more
#'    across the treatment groups or one of the treatment groups observed more than 3 AEs
#' - `filter = any(n > 2 & TRTA == "Xanomeline High Dose")`: keep AEs where the
#'    `"Xanomeline High Dose"` treatment group observed more than 2 AEs
#'
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
  if (length(by) > 0 && sum(is_overall) > 0) {
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
