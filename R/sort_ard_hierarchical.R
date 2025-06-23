#' Sort Stacked Hierarchical ARDs
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to sort stacked hierarchical ARDs.
#'
#' For the purposes of this function, we define a "variable group" as a combination of ARD rows grouped by the
#' combination of all their variable levels, but excluding any `by` variables.
#'
#' @param x (`card`)\cr
#'   a stacked hierarchical ARD of class `'card'` created using [ard_stack_hierarchical()] or
#'   [`ard_stack_hierarchical_count()`].
#' @param sort (`string`)\cr
#'   type of sorting to perform. Value must be one of:
#'   - `"alphanumeric"` - within each hierarchical section of the ARD, groups are ordered alphanumerically (i.e. A to Z)
#'     by `variable_level` text.
#'   - `"descending"` - within each variable group of the ARD, count sums are calculated for each group and groups are
#'     sorted in descending order by sum. If `sort = "descending"`, the `n` statistic is used to calculate variable
#'     group sums if included in `statistic` for all variables, otherwise `p` is used. If neither `n` nor `p` are
#'     present in `x` for all variables, an error will occur.
#'
#'   Defaults to `"descending"`.
#'
#' @return an ARD data frame of class 'card'
#' @seealso [filter_ard_hierarchical()]
#' @name sort_ard_hierarchical
#'
#' @note
#' If overall data is present in `x` (i.e. the ARD was created with `ard_stack_hierarchical(overall=TRUE)`), the
#' overall data will be sorted last within each variable group (i.e. after any other rows with the same combination of
#' variable levels).
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' ard_stack_hierarchical(
#'   ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = ADSL,
#'   id = USUBJID
#' ) |>
#'   sort_ard_hierarchical("alphanumeric")
#'
#' ard_stack_hierarchical_count(
#'   ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = ADSL
#' ) |>
#'   sort_ard_hierarchical("descending")
NULL

#' @rdname sort_ard_hierarchical
#' @export
sort_ard_hierarchical <- function(x, sort = c("descending", "alphanumeric")) {
  set_cli_abort_call()

  # check and process inputs ---------------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(sort)
  check_class(x, "card")
  if (!"args" %in% names(attributes(x))) {
    cli::cli_abort(
      paste(
        "Sorting is only available for stacked hierarchical ARDs created using",
        "{.fun ard_stack_hierarchical} or {.fun ard_stack_hierarchical_count}."
      ),
      call = get_cli_abort_call()
    )
  }
  sort <- arg_match(sort, error_call = get_cli_abort_call())

  x_args <- attributes(x)$args
  by_cols <- if (length(x_args$by) > 0) {
    paste0("group", seq_along(length(x_args$by)), c("", "_level"))
  } else {
    NULL
  }

  # for calculations by highest severity, innermost variable is extracted from by
  if (length(x_args$by) > 1) {
    x_args$variables <- c(x_args$variables, x_args$by[-1])
    x_args$include <- c(x_args$include, x_args$by[-1])
    x_args$by <- x_args$by[-1]
  }

  outer_cols <- if (length(x_args$variables) > 1) {
    x_args$variables |>
      utils::head(-1) |>
      stats::setNames(
        x |>
          dplyr::select(cards::all_ard_groups("names"), -all_of(by_cols)) |>
          names()
      )
  } else {
    NULL
  }

  # reformat ARD for sorting ---------------------------------------------------------------------
  x_sort <- x |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    .ard_reformat_sort(sort, x_args$by, outer_cols)

  if (sort == "alphanumeric") {
    # alphanumeric sort --------------------------------------------------------------------------
    sort_cols <- c(
      x |>
        dplyr::select(all_ard_groups(), -all_of(by_cols[c(FALSE, TRUE)])) |>
        names(),
      "variable",
      "variable_level"
    )

    # sort alphanumerically and get index order
    idx_sorted <- x_sort |>
      dplyr::arrange(dplyr::pick(all_of(sort_cols))) |>
      dplyr::pull("idx")
  } else {
    # descending sort ----------------------------------------------------------------------------
    # all variables in x have n or p stat present (not required if filtered out first)
    n_all <- length(setdiff(
      intersect(x_args$include, x$variable),
      x |> dplyr::filter(.data$stat_name == "n") |> dplyr::pull("variable")
    )) ==
      0
    if (!n_all) {
      p_all <- length(setdiff(
        intersect(x_args$include, x$variable),
        x |> dplyr::filter(.data$stat_name == "p") |> dplyr::pull("variable")
      )) ==
        0
      if (!p_all) {
        cli::cli_abort(
          paste(
            "If {.code sort='descending'} then either {.val n} or {.val p} must be present in {.arg x} for all",
            "variables in order to calculate the count sums used for sorting."
          ),
          call = get_cli_abort_call()
        )
      }
    }
    sort_stat <- if (n_all) "n" else "p"

    # calculate sums for each hierarchy level section/row
    x_sort <- x_sort |>
      .append_hierarchy_sums(by_cols, outer_cols, x_args$include, sort_stat)

    sort_cols <- c(
      by_cols[c(TRUE, FALSE)],
      rbind(
        x_sort |>
          dplyr::select(all_ard_groups("names"), -all_of(by_cols)) |>
          names(),
        x_sort |> dplyr::select(dplyr::starts_with("sum_group")) |> names(),
        x_sort |>
          dplyr::select(all_ard_groups("levels"), -all_of(by_cols)) |>
          names()
      ),
      "variable",
      "sum_row",
      "variable_level"
    )

    # sort by descending row sum and get index order
    idx_sorted <- x_sort |>
      dplyr::arrange(across(
        all_of(sort_cols),
        .fns = ~ (if (is.numeric(.x)) dplyr::desc(.x) else .x)
      )) |>
      dplyr::pull("idx")
  }

  x <- x[idx_sorted, ]

  # keep attributes at bottom of ARD
  idx_attr <- x$context == "attributes"
  x <- dplyr::bind_rows(x[!idx_attr, ], x[idx_attr, ])

  x
}

# this function reformats a hierarchical ARD for sorting
.ard_reformat_sort <- function(x, sort, by, outer_cols) {
  # reformat data from overall column (if present)
  is_overall_col <- apply(x, 1, function(x) {
    !isTRUE(any(x %in% by)) || x$context == "attributes"
  })
  if (sum(is_overall_col) > 0 && length(by) > 0) {
    x_overall_col <- x[is_overall_col, ] |>
      cards::rename_ard_groups_shift(shift = length(by)) |>
      dplyr::mutate(
        group1 = by[1],
        group1_level = list("..overall..")
      ) |>
      dplyr::select(any_of(names(x)))
    x <- dplyr::bind_rows(x[!is_overall_col, ], x_overall_col)
  }

  x <- x |>
    dplyr::group_by(.data$variable) |>
    dplyr::group_split() |>
    # fill in variable/variable_level in their corresponding grouping columns
    map(function(dat) {
      cur_var <- dat$variable |>
        unique() |>
        as.character()
      grp_match <- names(which(outer_cols == cur_var))
      if (length(grp_match) > 0) {
        dat |>
          dplyr::mutate(
            !!grp_match := ifelse(
              is.na(dat[[grp_match]]),
              cur_var,
              dat[[grp_match]]
            ),
            !!paste0(grp_match, "_level") := ifelse(
              is.na(dat[[grp_match]]),
              if (is.logical(unlist(dat$variable_level))) {
                list(NA)
              } else {
                dat$variable_level
              },
              dat[[paste0(grp_match, "_level")]]
            ),
            variable = if (sort == "alphanumeric") {
              "..empty.."
            } else {
              .data$variable
            }
          )
      } else if (cur_var == "..ard_hierarchical_overall..") {
        dat |>
          dplyr::mutate(
            group1 = "..overall..",
            variable_level = list("..overall..")
          )
      } else if (cur_var == "..ard_total_n..") {
        dat |>
          dplyr::mutate(
            group1 = "..empty..",
            group1_level = list(NA),
            variable = NA,
          )
      } else {
        dat
      }
    }) |>
    dplyr::bind_rows() |>
    tidyr::unnest(all_of(c(
      cards::all_ard_groups("levels"),
      cards::all_ard_variables("levels")
    ))) |>
    # summary row groups remain at the top of each sub-section when sorting
    dplyr::mutate(across(
      c(all_ard_groups("names")),
      .fns = ~ tidyr::replace_na(., "..empty..")
    ))

  x
}

# this function calculates and appends n sums for each hierarchy level section/row (across by variables)
.append_hierarchy_sums <- function(x, by_cols, outer_cols, include, sort_stat) {
  g_vars <- c()

  # calculate sums at each outer hierarchy level
  for (g in names(outer_cols)) {
    g_vars <- c(g_vars, g, paste0(g, "_level"))

    # if variable not in include, use the sums from the next variable available
    if (outer_cols[g] %in% include) {
      next_incl <- outer_cols[g]
      var_nm <- "variable"
    } else {
      inner_var <- dplyr::last(include)
      next_incl <- intersect(
        c(
          outer_cols[which(names(outer_cols) == g):length(outer_cols)],
          inner_var
        ),
        include
      )[1]
      var_nm <- if (next_incl == inner_var) {
        "variable"
      } else {
        names(outer_cols)[which(outer_cols == next_incl)]
      }
    }
    g_sums <- x |>
      dplyr::filter(
        .data$stat_name == sort_stat,
        .data[[var_nm]] == next_incl
      ) |>
      dplyr::group_by(across(all_of(g_vars))) |>
      dplyr::summarize(
        !!paste0("sum_", g) := sum(unlist(.data$stat[
          .data$stat_name == sort_stat
        ]))
      )

    # append sums to each row
    x <- x |> dplyr::left_join(g_sums, by = g_vars)

    # remove variable name for outer hierarchy variables
    x$variable[x$variable == outer_cols[g]] <- "..empty.."
  }

  # append row sums for every row (across by variables)
  x <- x |>
    dplyr::group_by(across(c(
      all_ard_groups(),
      all_ard_variables(),
      -all_of(by_cols)
    ))) |>
    dplyr::reframe(
      across(everything()),
      sum_row = sum(unlist(.data$stat[.data$stat_name == sort_stat]))
    )

  x
}
