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
#'   sort_ard_hierarchical(sort = list(AESOC ~ "alphanumeric", AEDECOD ~ "descending"))
NULL

#' @rdname sort_ard_hierarchical
#' @export
sort_ard_hierarchical <- function(x, sort = everything() ~ "descending") {
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

  if (all(x$variable %in% "..ard_hierarchical_overall..")) {
    return(x)
  }

  x_args <- attributes(x)$args

  # get and check name of sorting variables
  if (is.character(sort)) {
    sort <- as.formula(paste0("everything() ~ '", sort, "'"))
  }
  process_formula_selectors(
    as.list(x_args$variables) |> data.frame() |> stats::setNames(x_args$variables),
    sort = sort
  )
  fill_formula_selectors(
    as.list(x_args$variables) |> data.frame() |> stats::setNames(x_args$variables),
    sort = everything() ~ "descending"
  )
  if (!all(unlist(sort) %in% c("descending", "alphanumeric"))) {
    cli::cli_abort(
      "Sorting type must be either {.val descending} or {.val alphanumeric} for all variables.",
      call = get_cli_abort_call()
    )
  }

  # for calculations by highest severity, innermost variable is extracted from `by`
  if (length(x_args$by) > 1) {
    x_args$variables <- c(x_args$variables, x_args$by[-1])
    x_args$include <- c(x_args$include, x_args$by[-1])
    x_args$by <- x_args$by[-1]
  }

  by <- x_args$by
  cols <-
    x_args$variables |>
    stats::setNames(
      x |>
        dplyr::select(all_ard_group_n(seq_along(x_args$variables) + length(by), types = "names"), "variable") |>
        names()
    )

  # attributes and total n not sorted - appended to bottom of sorted ARD
  has_attr <- "attributes" %in% x$context | "total_n" %in% x$context
  if (has_attr) {
    x_attr <- x |>
      dplyr::filter(context %in% c("attributes", "total_n"))
    x <- x |>
      dplyr::filter(!context %in% c("attributes", "total_n"))
  }

  # reformat ARD for sorting ---------------------------------------------------------------------
  x_sort <- x |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    dplyr::mutate(
      variable_level =
        if_else(.data$variable %in% c("..ard_hierarchical_overall..", "..ard_total_n.."), list(NA), variable_level)
    )

  for (i in seq_along(x_args$variables)) {
    sort_i <- sort[[cols[i]]]
    cur_var <- names(cols)[i]

    x_sort <- x_sort |>
      .ard_reformat_sort(cur_var, by, cols, i)

    if (sort_i == "descending") {
      # descending sort
      x_sort <- x_sort |>
        .append_hierarchy_sums(x_args, cur_var, i)
    } else {
      # alphanumeric sort
      x_sort <- x_sort |>
        dplyr::arrange(.by_group = TRUE) |>
        dplyr::mutate(!!paste0("sort_group_", i) := dplyr::cur_group_id())
    }
  }

  # apply sorting at each hierarchy level
  idx_sorted <- x_sort |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::pick(starts_with("sort_group_"))) |>
    dplyr::pull(idx)
  x <- x[idx_sorted, ]

  # keep attributes at bottom of ARD
  if (has_attr) x <- dplyr::bind_rows(x, x_attr)

  x
}

# this function reformats a hierarchical ARD for sorting
.ard_reformat_sort <- function(x, cur_var, by, cols, i) {
  cur_var_lvl <- paste0(cur_var, "_level")

  # header info
  if (!is_empty(by) & i == 1) {
    x <- x |>
      dplyr::mutate(
        !!cur_var := if_else(!is_empty(by) & .data$variable %in% by, "..empty..", .data[[cur_var]]),
        !!cur_var_lvl := if_else(!is_empty(by) & .data$variable %in% by, as.list(NA), .data[[cur_var_lvl]])
      )
  }

  # group summary rows
  if (!cur_var %in% "variable") {
    x[x$variable %in% cols[i], ] <-
      x[x$variable %in% cols[i], ] |>
      dplyr::mutate(
        !!cur_var := .data$variable,
        !!cur_var_lvl := .data$variable_level,
        variable := "..overall..",
        variable_level = as.list(NA)
      )
  }
  # overall=TRUE rows
  if (!is_empty(by) & !cur_var %in% "variable" & any(x[[paste0("group", i)]] %in% cols[i])) {
    x[x[[paste0("group", i)]] %in% cols[i], ] <-
      x[x[[paste0("group", i)]] %in% cols[i], ] |>
      dplyr::mutate(
        !!paste0("group", i + length(by) + 1) := .data[[cur_var]],
        !!paste0("group", i + length(by) + 1, "_level") := .data[[cur_var_lvl]],
        !!cur_var := .data[[paste0("group", i)]],
        !!cur_var_lvl := as.list(.data[[paste0("group", i, "_level")]])
      )
  }
  # overall summary rows
  x[is.na(x[[cur_var]]), ] <-
    x[is.na(x[[cur_var]]), ] |>
    dplyr::mutate(
      !!cur_var :=
        dplyr::case_when(
          .data$variable == "..ard_hierarchical_overall.." ~ "..overall..",
          .data$group1 %in% c(by, NA) ~ "..empty..",
          .default = NA
        ),
      !!cur_var_lvl := as.list(NA)
    )

  x |>
    dplyr::mutate(dplyr::across(cur_var_lvl, ~ .x |> unlist())) |>
    dplyr::group_by(pick(any_of(cards::all_ard_group_n(seq_len(i) + length(by))), any_of(c(cur_var, cur_var_lvl))))
}

# this function calculates and appends n sums for each hierarchy level section/row (across by variables)
.append_hierarchy_sums <- function(x, x_args, cur_var, i) {
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

  sum_i <- paste0("sum_group_", i)

  x_sums <- x |>
    dplyr::filter(
      .data$stat_name == sort_stat,
      (variable == dplyr::last(x_args$variables) | .data[[cur_var]] %in% c("..empty..", "..overall..")),
      if (!is_empty(x_args$by)) !.data$group1 %in% x_args$variables
    ) |>
    dplyr::summarize(!!sum_i := sum(unlist(.data$stat[.data$stat_name == sort_stat]))) |>
    ungroup()

  if (i == 1) {
    x_sums[x_sums[[cur_var]] %in% "..overall..", sum_i] <- max(x_sums[[sum_i]], na.rm = TRUE) + 1
    x_sums[x_sums[[cur_var]] %in% "..empty..", sum_i] <- max(x_sums[[sum_i]], na.rm = TRUE) + 1
  }

  sort_cols <- append(dplyr::group_vars(x), sum_i, after = length(dplyr::group_vars(x)) - 2)
  x_sums <- x_sums |>
    dplyr::arrange(across(sort_cols, \(x) if (is.numeric(x)) desc(x) else x)) |>
    dplyr::mutate(!!paste0("sort_group_", i) := dplyr::row_number())

  x |>
    left_join(x_sums, by = group_vars(x))
}
