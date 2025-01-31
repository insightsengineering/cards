#' Hierarchical ARD Statistics
#'
#' `r lifecycle::badge('experimental')`\cr
#' Performs hierarchical or nested tabulations, e.g. tabulates AE terms
#' nested within AE system organ class.
#' - `ard_hierarchical()` includes summaries for the last variable listed
#'   in the `variables` argument, nested within the other variables included.
#' - `ard_hierarchical_count()` includes summaries for _all_ variables
#'   listed in the `variables` argument each summary nested within the preceding
#'   variables, e.g. `variables=c(AESOC, AEDECOD)` summarizes `AEDECOD` nested
#'   in `AESOC`, and also summarizes the counts of `AESOC`.
#'
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables to perform the nested/hierarchical tabulations within.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables to perform tabulations by. All combinations of the variables
#'   specified here appear in results. Default is `dplyr::group_vars(data)`.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   an optional argument used to assert there are no duplicates within
#'   the `c(id, variables)` columns.
#' @inheritParams ard_categorical
#'
#' @return an ARD data frame of class 'card'
#' @name ard_hierarchical
#'
#' @inheritSection ard_categorical Denominators
#'
#' @examples
#' ard_hierarchical(
#'   data = ADAE |>
#'     dplyr::slice_tail(n = 1L, by = c(USUBJID, TRTA, AESOC, AEDECOD)),
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   id = USUBJID,
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM)
#' )
#'
#' ard_hierarchical_count(
#'   data = ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA
#' )
NULL

#' @rdname ard_hierarchical
#' @export
ard_hierarchical <- function(data, ...) {
  check_not_missing(data)
  UseMethod("ard_hierarchical")
}

#' @rdname ard_hierarchical
#' @export
ard_hierarchical_count <- function(data, ...) {
  check_not_missing(data)
  UseMethod("ard_hierarchical_count")
}

#' @rdname ard_hierarchical
#' @export
ard_hierarchical.data.frame <- function(data,
                                        variables,
                                        by = dplyr::group_vars(data),
                                        statistic = everything() ~ c("n", "N", "p"),
                                        denominator = NULL, fmt_fn = NULL,
                                        stat_label = everything() ~ default_stat_labels(),
                                        id = NULL,
                                        ...) {
  set_cli_abort_call()
  check_dots_used()

  # check inputs ---------------------------------------------------------------
  check_not_missing(variables)

  # process arguments ----------------------------------------------------------
  process_selectors(
    data,
    variables = {{ variables }},
    by = {{ by }},
    id = {{ id }}
  )
  data <- dplyr::ungroup(data)

  if (!is_empty(id) && anyDuplicated(data[c(id, variables)]) > 0L) {
    cli::cli_warn(c(
      "Duplicate rows found in data for the {.val {id}} column{?s}.",
      "i" = "Percentages/Denominators are not correct."
    ))
  }

  # return empty ARD if no variables selected ----------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble() |> as_card())
  }

  # if denominator doesn't have all by, they need to be added ------------------
  if (!is.null(denominator) && is.data.frame(denominator) && !all(by %in% names(denominator))) {
    by_vars_not_present <- by |> setdiff(names(denominator))
    denominator <-
      data |>
      dplyr::select(all_of(by_vars_not_present)) |>
      dplyr::distinct() |>
      dplyr::mutate(
        ...ard_data_column... = list(denominator)
      ) |>
      tidyr::unnest(cols = "...ard_data_column...")
  }

  # add dummy variable for counting --------------------------------------------
  data[["...ard_dummy_for_counting..."]] <- 1L

  # perform tabulations --------------------------------------------------------
  df_result <-
    ard_categorical(
      data = data,
      variables = "...ard_dummy_for_counting...",
      by = all_of(by),
      strata = all_of(variables),
      statistic = statistic,
      denominator = denominator,
      fmt_fn = fmt_fn,
      stat_label = stat_label
    )

  # renaming columns -----------------------------------------------------------
  df_result <- .rename_last_group_as_variable(df_result)

  # return ard -----------------------------------------------------------------
  df_result |>
    dplyr::mutate(context = "hierarchical")
}

#' @rdname ard_hierarchical
#' @export
ard_hierarchical_count.data.frame <- function(data,
                                              variables,
                                              by = dplyr::group_vars(data),
                                              fmt_fn = NULL,
                                              stat_label = everything() ~ default_stat_labels(),
                                              sort = NULL,
                                              ...) {
  set_cli_abort_call()
  check_dots_used()

  # check inputs ---------------------------------------------------------------
  check_not_missing(variables)

  # process arguments ----------------------------------------------------------
  process_selectors(data, variables = {{ variables }}, by = {{ by }})

  # return empty ARD if no variables selected ----------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble() |> as_card())
  }

  # add dummy variable for counting --------------------------------------------
  data[["...ard_dummy_for_counting..."]] <- 1L

  # perform tabulations --------------------------------------------------------
  df_result <- ard_categorical(
    data = data,
    variables = "...ard_dummy_for_counting...",
    by = all_of(by),
    strata = all_of(variables),
    statistic = everything() ~ "n",
    fmt_fn = fmt_fn,
    stat_label = stat_label
  ) |>
    .rename_last_group_as_variable() |>
    dplyr::mutate(context = "hierarchical_count") |>
    as_card()

  if (!is.null(sort) && sort == "descending") {
    by_cols <- paste0("group", seq_along(length(by)), c("", "_level"))
    df_sums <- df_result |>
      dplyr::filter(.data$stat_name == "n") |>
      dplyr::group_by(across(c(all_ard_groups(), all_ard_variables(), -all_of(by_cols))))
    gp_vars <- df_sums |> dplyr::group_vars()
    cur_gp <- paste0("sum_group", length(by) + length(variables))
    df_sums <- df_sums |>
      dplyr::summarise(
        sum_row = sum(unlist(.data$stat))#,
        # !!cur_gp := sum(unlist(.data$stat))
      ) #|>
      # dplyr::summarize(!!cur_gp := dplyr::first(.data$sum_row)) |>
      # dplyr::ungroup() |>
      # dplyr::rename(label = "variable_level") |>
      # tidyr::unnest(cols = everything())

    df_result <- df_result |>
      dplyr::left_join(
        df_sums,
        # result |>
          # dplyr::summarize(!!paste0("sum_", g) := dplyr::first(.data$sum_row)),
        by = gp_vars
      )

    # df_result <- df_result |>
      # dplyr::group_by(across(c(all_ard_groups("levels"), all_ard_variables("levels"), -dplyr::all_of(by_cols)))) |>
      # dplyr::summarize(!!paste0("sum_", g) := dplyr::first(.data$sum_row))
  }

  df_result
}

#' Rename Last Group to Variable
#'
#' In the `ard_hierarchical*()` functions, the last grouping variable is
#' renamed to `variable` and `variable_level` before being returned.
#'
#' @param df_result (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#'
#' @return an ARD data frame of class 'card'
#' @keywords internal
#'
#' @examples
#' data <- data.frame(x = 1, y = 2, group1 = 3, group2 = 4)
#'
#' cards:::.rename_last_group_as_variable(data)
.rename_last_group_as_variable <- function(df_result) {
  df_result <- dplyr::select(df_result, -all_ard_variables())

  last_two_grouping_columns <-
    dplyr::select(df_result, all_ard_groups()) |>
    names() |>
    utils::tail(n = 2L) |>
    stats::setNames(c("variable", "variable_level"))

  dplyr::rename(df_result, !!!last_two_grouping_columns)
}
