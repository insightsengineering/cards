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
#' @param var ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   hierarchy variable from `x` to perform filtering on. If `NULL`, the last hierarchy variable
#'   from `x` (`dplyr::last(attributes(x)$args$variables)`) will be used.
#' @param keep_empty (scalar `logical`)\cr
#'   Logical argument indicating whether to retain summary rows corresponding to hierarchy
#'   sections that have had all rows filtered out. Default is `FALSE`.
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
#'     denominator = ADSL,
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
#'   denominator = ADSL,
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
#' # Keep AEs from SOCs that have an overall prevalence of greater than 20%
#' filter_ard_hierarchical(ard, sum(n) / sum(N) > 0.20, var = AESOC)
NULL

#' @rdname filter_ard_hierarchical
#' @export
filter_ard_hierarchical <- function(x, filter, var = NULL, keep_empty = FALSE) {
  set_cli_abort_call()

  # check and process inputs ---------------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(filter)
  check_scalar_logical(keep_empty)
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
  by <- ard_args$by

  # get name of filtering variable
  process_selectors(
    as.list(ard_args$variables) |> data.frame() |> stats::setNames(ard_args$variables),
    var = {{ var }}
  )
  if (is_empty(var)) var <- dplyr::last(ard_args$variables)
  which_var <- which(ard_args$variables == var)

  filter <- enquo(filter)
  if (!quo_is_call(filter)) {
    cli::cli_abort(
      "The {.arg filter} argument must be an expression.",
      call = get_cli_abort_call()
    )
  }
  if (!var %in% ard_args$include) {
    cli::cli_abort(
      paste(
        "No statistics available in the ARD for variable {.val {var}}. In order to filter on {.val {var}}",
        "it must be specified in the {.arg include} argument when the ARD is created."
      ),
      call = get_cli_abort_call()
    )
  }

  by_cols <- if (!is_empty(by)) c("group1", "group1_level") else NULL
  filter_vars <- all.vars(filter)
  valid_filter_vars <- c(unique(x$stat_name[x$variable == var]), by)
  if (!all(filter_vars %in% valid_filter_vars)) {
    var_miss <- setdiff(filter_vars, valid_filter_vars)
    cli::cli_abort(
      c(
        paste(
          "The expression provided as {.arg filter} includes condition{?s} for statistic{?s}",
          "{.val {var_miss}} which {?is/are} not present in the ARD and {?does/do} not",
          "correspond to any of the {.var by} variable levels."
        ),
        i = "Valid filter terms for variable {.val {var}} are: {.val {valid_filter_vars}}."
      ),
      call = get_cli_abort_call()
    )
  }

  # ignore "overall" data
  is_overall <- apply(x, 1, function(x) !isTRUE(any(x %in% by)))
  if (!is_empty(by) && sum(is_overall) > 0) {
    x <- x[!is_overall, ]
  }

  # reshape ARD so each stat is in its own column ------------------------------------------------
  x_f <- x |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    dplyr::select(
      all_ard_groups(),
      all_ard_variables(),
      "stat_name",
      "stat",
      "idx"
    ) |>
    tidyr::pivot_wider(
      id_cols = c(all_ard_groups(), all_ard_variables()),
      names_from = "stat_name",
      values_from = "stat",
      values_fn = unlist,
      unused_fn = list
    )

  # apply filter ---------------------------------------------------------------------------------
  f_idx <- x_f |>
    dplyr::group_by(across(c(
      all_ard_groups(),
      all_ard_variables(),
      -all_of(by_cols)
    ))) |>
    dplyr::group_map(\(.df, .g) {
      # only filter rows for variable `var`
      if (.g$variable == var) {
        if (!is_empty(by)) {
          # use `by` variable name as `group1_level` column name
          names(.df)[names(.df) == by_cols[c(FALSE, TRUE)]] <- by
        }

        # apply filter
        .df[["idx"]][any(eval_tidy(filter, data = .df), na.rm = TRUE)]
      } else {
        .df[["idx"]]
      }
    }) |>
    unlist() |>
    sort()
  x <- x[f_idx, ]

  # remove inner variable rows if `var` is an outer variable that does not meet the filter criteria
  if (which_var < length(ard_args$variables)) {
    var_gp_nm <- paste0("group", length(by) + which_var) # get `var` group variable name

    # get all combos of variables kept after filtering
    # keep only unique combos up to `var` group variable
    var_keep <- x |>
      dplyr::filter(variable == var) |>
      dplyr::mutate(
        !!var_gp_nm := .data$variable,
        !!paste0(var_gp_nm, "_level") := .data$variable_level
      )
    var_keep <- dplyr::distinct(var_keep[1:((length(by) + which_var)*2)])

    # track row indices
    x <- x |> dplyr::mutate(idx = dplyr::row_number())

    # get row indices to exclude - all rows within `var` sections that have been removed
    f_idx_inner <-
      dplyr::anti_join(
        x[x[[var_gp_nm]] == var & !is.na(x[[var_gp_nm]]), ],
        var_keep,
        by = names(var_keep)
      ) |>
      dplyr::pull("idx")

    # filter out inner rows
    x <- x |>
      dplyr::filter(!.data$idx %in% f_idx_inner) |>
      dplyr::select(-"idx")
  }

  # remove summary rows from empty sections if requested
  if (var != ard_args$variables[1] && !keep_empty && length(ard_args$include) > 1) {
    outer_cols <- ard_args$variables |> utils::head(-1)
    # if all inner rows filtered out, remove all summary rows - only overall/header rows left
    if (!dplyr::last(ard_args$variables) %in% x$variable) {
      x <- x |> dplyr::filter(!.data$variable %in% outer_cols)
    } else {
      names(outer_cols) <- x |>
        dplyr::select(all_ard_groups("names"), -all_of(by_cols)) |>
        names()
      x_no_sum <- x |>
        dplyr::mutate(idx = dplyr::row_number()) |>
        .ard_reformat_sort("no_sort", by, outer_cols)
      # check if each hierarchy section (from innermost to outermost) is empty and if so remove its summary row
      for (i in rev(seq_along(outer_cols))) {
        x_no_sum <- x_no_sum |>
          dplyr::group_by(across(c(all_ard_group_n((length(by):i) + 1), -all_of(by_cols)))) |>
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

  as_card(x)
}
