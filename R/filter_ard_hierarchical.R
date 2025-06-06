#' Filter Stacked Hierarchical ARDs
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to filter stacked hierarchical ARDs.
#'
#' For the purposes of this function, we define a "variable group" as a combination of ARD rows
#' grouped by the combination of all their variable levels, but excluding any `by` variables.
#'
#' @param x (`card`)\cr
#'   a stacked hierarchical ARD of class `'card'` created using [`ard_stack_hierarchical()`] or
#'   [`ard_stack_hierarchical_count()`].
#' @param filter (`expression`)\cr
#'   an expression that is used to filter variable groups of the hierarchical ARD. See the
#'   Details section below.
#' @param keep_empty (scalar `logical`)\cr
#'   Logical argument indicating whether to retain summary rows corresponding to hierarchy
#'   sections that have had all rows filtered out. Default is `FALSE`.
#' @param diff_by (`character`)\cr
#'   a vector of length 2 or more containing names of the levels of the `by` variable from `x` to compute the
#'   _difference_ of when filtering. The first element specified will be used as the reference group to which each of
#'   the other (comparison) groups will be compared. If this argument is specified, the statistic given in `filter` will
#'   be computed in each variable group separately for each value of `diff_by` and then the absolute difference of
#'   reference group with each of the comparison groups is compared to determine whether the variable group should be
#'   filtered out. If any of the difference values meet the filtering criteria, the variable group is kept. If `NULL`
#'   (default), the filter will be applied as usual without considering differences. Aggregate functions should not be
#'   used in `filter` when `diff_by` is specified.
#'
#'   For example, if `filter = n / N > 0.05` and `diff_by = c("ARM X", "ARM Y", "ARM Z")`, statistic `n / N` will be
#'   computed within variable groups for each value of `diff_by` - say `stat_X`, `stat_Y`, and `stat_Z` - and then each
#'   variable group will be kept only if `abs(stat_X - stat_Y) > 0.05` _or_ `abs(stat_X - stat_Z) > 0.05`.
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
#' Filters are applied to the summary statistics of the innermost variable in the hierarchy---`AEDECOD`
#'   in this case.
#' If any of the summary statistics meet the filter requirement for any of the treatment groups,
#'   the entire row is retained.
#' For example, if `filter = n >= 9` were passed, the criteria would be met for DIARRHOEA
#'   as the Placebo group observed 9 AEs and as a result the summary statistics for the other
#'   treatment groups would be retained as well.
#' Conversely, no treatment groups' summary statistics satisfy the filter requirement
#'   for VOMITING so all rows associated with this AE would be removed.
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
#' If `ard_stack_hierarchical(over_variables=TRUE)` was run, any overall statistics are kept regardless
#' of filtering.
#'
#' Some examples of possible filters:
#' - `filter = n > 5`: keep AEs where one of the treatment groups observed more than 5 AEs
#' - `filter = n == 2 & p < 0.05`: keep AEs where one of the treatment groups observed exactly 2
#'    AEs _and_ one of the treatment groups observed a proportion less than 5%
#' - `filter = sum(n) >= 4`: keep AEs where there were 4 or more AEs observed across the treatment groups
#' - `filter = mean(n) > 4 | n > 3`: keep AEs where the mean number of AEs is 4 or more across the
#'   treatment groups _or_ one of the treatment groups observed more than 3 AEs
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
#'
#' # Example 4 ----------------------------------
#' # Keep AEs that have a difference in prevalence of greater than 3% between reference group with
#' # `TRTA = "Xanomeline High Dose"` and comparison group with `TRTA = "Xanomeline Low Dose"`
#' filter_ard_hierarchical(ard, n / N > 0.03, diff_by = c("Xanomeline High Dose", "Xanomeline Low Dose"))
NULL

#' @rdname filter_ard_hierarchical
#' @export
filter_ard_hierarchical <- function(x, filter, keep_empty = FALSE, diff_by = NULL) {
  set_cli_abort_call()

  # check and process inputs ---------------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(filter)
  check_scalar_logical(keep_empty)
  check_class(diff_by, "character", allow_empty = TRUE)
  check_class(x, "card")
  if (!"args" %in% names(attributes(x))) {
    cli::cli_abort(
      paste(
        "Filtering is only available for stacked hierarchical ARDs created using",
        "{.fun ard_stack_hierarchical} or {.fun ard_stack_hierarchical_count}."
      ),
      call = get_cli_abort_call()
    )
  }
  ard_args <- attributes(x)$args

  filter <- enquo(filter)
  if (!quo_is_call(filter)) {
    cli::cli_abort(
      "The {.arg filter} argument must be an expression.",
      call = get_cli_abort_call()
    )
  }

  by_cols <- if (length(ard_args$by) > 0) paste0("group", seq_along(ard_args$by), c("", "_level")) else NULL
  if (!all(all.vars(filter) %in% c(x$stat_name, ard_args$by))) {
    var_miss <- setdiff(all.vars(filter), c(x$stat_name, ard_args$by))
    cli::cli_abort(
      paste(
        "The expression provided as {.arg filter} includes condition{?s} for statistic{?s} or `by` variable{?s}",
        "{.val {var_miss}} which {?is/are} not present in the ARD."
      ),
      call = get_cli_abort_call()
    )
  }
  if (!is_empty(diff_by)) {
    if (is_empty(ard_args$by)) {
      cli::cli_abort(
        "A {.arg by} variable must have been specified in order to use the {.arg diff_by} argument.",
        call = get_cli_abort_call()
      )
    } else {
      by_levels <- unique(unlist(x[x[["group1"]] == ard_args$by, ][["group1_level"]]))
      if (length(diff_by) < 2) {
        cli::cli_abort(
          "{.arg diff_by} must contain at least 2 levels of {.arg by} to compare.",
          call = get_cli_abort_call()
        )
      }
      if (!all(diff_by %in% by_levels)) {
        not_by_levels <- setdiff(diff_by, by_levels)
        cli::cli_abort(
          paste(
            "{.arg diff_by} contains element{?s} {not_by_levels} which are not valid levels of {.arg by}",
            "variable {.val ard_args$by}."
          ),
          call = get_cli_abort_call()
        )
      }
    }
  }

  # ignore "overall" data
  is_overall <- apply(x, 1, function(x) !isTRUE(any(x %in% ard_args$by)))
  if (length(ard_args$by) > 0 && sum(is_overall) > 0) {
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
      # only filter rows for innermost variable
      if (.g$variable == dplyr::last(attributes(x)$args$variables)) {
        # allow filtering on `by` variable levels
        if (length(ard_args$by) > 0) {
          names(.df)[names(.df) == by_cols[c(FALSE, TRUE)]] <- ard_args$by
        }
        if (is_empty(diff_by)) {
          .df[["idx"]][eval_tidy(filter, data = .df)]
        } else {
          # calculate statistic to be compared for each column in `diff_by`
          stat_vals <- .df |>
            dplyr::mutate(stat_diff = eval_tidy(filter[[2]][[2]], data = .df)) |>
            dplyr::filter(.data[[ard_args$by]] %in% diff_by) |>
            dplyr::pull(stat_diff, name = .data[[ard_args$by]])
          ref <- which(names(stat_vals) == diff_by[1])
          # compare comp statistic values with ref statistic value using the given comparison criteria
          is_diff <- eval(parse(
            text = paste(abs(stat_vals[ref] - stat_vals[-ref]), filter[[2]][1], filter[[2]][3], collapse = " | ")
          ))
          if (is_diff) .df[["idx"]] else list()
        }
      } else {
        .df[["idx"]]
      }
    }) |>
    unlist() |>
    sort()
  x <- x[f_idx, ]

  # remove summary rows from empty sections if requested
  if (!keep_empty && length(ard_args$include) > 1) {
    outer_cols <- ard_args$variables |> utils::head(-1)
    # if all rows filtered out remove all summary rows - only overall/header rows left
    if (!dplyr::last(ard_args$variables) %in% x$variable) {
      x <- x |> dplyr::filter(!.data$variable %in% outer_cols)
    } else {
      names(outer_cols) <- x |>
        dplyr::select(all_ard_groups("names"), -all_of(by_cols)) |>
        names()
      x_no_sum <- x |>
        dplyr::mutate(idx = dplyr::row_number()) |>
        .ard_reformat_sort("no_sort", ard_args$by, outer_cols)
      # check if each hierarchy section (from innermost to outermost) is empty and if so remove its summary row
      for (i in rev(seq_along(outer_cols))) {
        x_no_sum <- x_no_sum |>
          dplyr::group_by(across(c(all_ard_group_n((length(ard_args$by):i) + 1), -all_of(by_cols)))) |>
          dplyr::group_map(
            function(.df, .y) {
              cur_var <- .y[[ncol(.y) - 1]]
              if (cur_var == "..empty..") {
                .df
              } else {
                inner_rows <- .df |> dplyr::filter(.data$variable != cur_var)
                if (nrow(inner_rows) > 0) .df else NULL
              }
            },
            .keep = TRUE
          ) |>
          dplyr::bind_rows()
      }
      idx_no_sum <- sort(x_no_sum$idx)
      x <- x[idx_no_sum, ]
    }
  }

  x
}
