#' Sort Stacked Hierarchical ARDs
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function is used to sort stacked hierarchical ARDs.
#'
#' @param x (`card`)\cr
#'   A stacked hierarchical ARD of class `'card'` created using [ard_stack_hierarchical()].
#' @param sort (`string` or `NULL`)\cr
#'   type of sorting to perform. Values must be one of:
#'   - `"alphanumeric"` - within each hierarchical section of the ARD, rows are ordered alphanumerically (i.e. A to Z)
#'     by `variable_label` text.
#'   - `"descending"` - within each hierarchical section of the ARD, count sums are calculated for each row and rows are
#'     sorted in descending order by sum. If `sort = "descending"`, the `n` statistic is used to calculate row sums if
#'     included in `statistic` for all variables, otherwise `p` is used. If neither `n` nor `p` are present in `x` for
#'     all variables, an error will occur.
#'   Defaults to `"alphanumeric"`.
#'
#' @return an ARD data frame of class 'card'
#' @seealso [ard_filter()]
#' @name ard_sort
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' ard_stack_hierarchical(
#'   ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM),
#'   id = USUBJID
#' ) |>
#'   ard_sort()
#'
#' ard_stack_hierarchical_count(
#'   ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM)
#' ) |>
#'   ard_sort("descending")
NULL

#' @rdname ard_sort
#' @export
ard_sort <- function(x, sort = "alphanumeric") {
  set_cli_abort_call()

  # check and process inputs ---------------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(sort)
  check_class(x, "card")
  if (!"args" %in% names(attributes(x))) {
    cli::cli_abort(
      "Sorting is only available for stacked hierarchical ARDs created using {.fun ard_stack_hierarchical}.",
      call = get_cli_abort_call()
    )
  }
  if (!sort %in% c("descending", "alphanumeric")) {
    cli::cli_abort(
      "The {.arg sort} argument must be either {.val descending} or {.val alphanumeric}.",
      call = get_cli_abort_call()
    )
  }

  by_cols <- paste0("group", seq_along(length(attributes(x)$args$by)), c("", "_level"))
  outer_cols <- attributes(x)$args$variables |>
    head(-1) |>
    setNames(x |> dplyr::select(cards::all_ard_groups("names"), -by_cols) |> names())

  # reformat ARD for sorting ---------------------------------------------------------------------
  x_sort <- x |>
    .ard_reformat_sort(sort, outer_cols) |>
    dplyr::mutate(idx = dplyr::row_number())

  if (sort == "alphanumeric") {
    # alphanumeric sort --------------------------------------------------------------------------
    sort_cols <- c(
      x |> dplyr::select(all_ard_groups(), -by_cols[c(FALSE, TRUE)]) |> names(),
      "variable", "variable_level"
    )

    # sort alphanumerically and get index order
    idx_sorted <- x_sort |>
      dplyr::arrange(across(all_of(sort_cols), ~.x)) |>
      dplyr::pull(idx)
  } else {
    # descending sort ----------------------------------------------------------------------------
    n_all <- all(attributes(x)$args$variables %in% (x |> dplyr::filter(stat_name == "n") |> dplyr::pull(variable)))
    if (!n_all) {
      p_all <- all(attributes(x)$args$variables %in% (x |> dplyr::filter(stat_name == "p") |> dplyr::pull(variable)))
      if (!p_all) {
        cli::cli_abort(
          paste(
            "If {.code sort='descending'} then either {.val n} or {.val p} must be present in {.arg x} for all",
            "variables in order to calculate the count sums used for sorting.",
          ),
          call = get_cli_abort_call()
        )
      }
    }
    sort_stat <- if (n_all) "n" else "p"

    # calculate sums for each hierarchy level section/row
    x_sort <- x_sort |> .append_hierarchy_sums(by_cols, outer_cols, sort_stat)

    sort_cols <- c(by_cols[c(TRUE, FALSE)], rbind(
      x_sort |> dplyr::select(dplyr::starts_with("sum_group")) |> names(),
      x_sort |> dplyr::select(all_ard_groups("names"), -by_cols) |> names(),
      x_sort |> dplyr::select(all_ard_groups("levels"), -by_cols) |> names()
    ), "sum_row", "variable_level")

    # sort by descending row sum and get index order
    idx_sorted <- x_sort |>
      dplyr::arrange(across(all_of(sort_cols), .fns = ~ (if (is.numeric(.x)) dplyr::desc(.x) else .x))) |>
      dplyr::pull(idx)
  }

  x[idx_sorted, ]
}

# this function reformats a hierarchical ARD for sorting
.ard_reformat_sort <- function(x, sort, outer_cols) {
  x |>
    dplyr::mutate(variable = fct_inorder(.data$variable)) |>
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
            !!grp_match := ifelse(is.na(dat[[grp_match]]), cur_var, dat[[grp_match]]),
            !!paste0(grp_match, "_level") := ifelse(
              is.na(dat[[grp_match]]), dat$variable_level, dat[[paste0(grp_match, "_level")]]
            ),
            variable = if (sort == "alphanumeric") " " else .data$variable
          )
      } else {
        dat
      }
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(variable = as.character(.data$variable)) |>
    # summary rows remain at the top of each sub-section when sorting
    dplyr::mutate(across(c(all_ard_groups("names")), .fns = ~ tidyr::replace_na(., " ")))
}

# this function calculates and appends n sums for each hierarchy level section/row (across by variables)
.append_hierarchy_sums <- function(x, by_cols, outer_cols, sort_stat) {
  g_vars <- c()

  # calculate sums at each outer hierarchy level
  for (g in names(outer_cols)) {
    g_vars <- c(g_vars, g, paste0(g, "_level"))
    g_sums <- x |>
      dplyr::filter(.data$stat_name == sort_stat, variable == outer_cols[g]) |>
      dplyr::group_by(across(g_vars)) |>
      dplyr::summarize(
        !!paste0("sum_", g) := sum(unlist(.data$stat[.data$stat_name == sort_stat]))
      )

    # append sums to each row
    x <- x |> dplyr::left_join(g_sums, by = g_vars)
  }

  # append row sums for every row (across by variables)
  x <- x |>
    dplyr::group_by(across(c(all_ard_groups(), all_ard_variables(), -all_of(by_cols)))) |>
    dplyr::reframe(across(everything()), sum_row = sum(unlist(.data$stat[.data$stat_name == sort_stat])))

  x
}
