#' Hierarchical ARD Statistics
#'
#' Performs hierarchical or nested tabulations, e.g. tabulates AE terms
#' nested within AE system organ class.
#' - `ard_hierarchical()` includes summaries for the last variable listed
#'   in the `variables` argument, nested within the other variables included.
#' - `ard_hierarchical_count()` includes summaries for _all_ variables
#'   listed in the `variables` argument each summary nested within the preceding
#'   variables, e.g. `variables=c(AESOC, AETERM)` summarizes `AETERM` nested
#'   in `AESOC`, and also summarizes the counts of `AESOC`.
#'
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables to perform the nested/hierarchical tabulations within.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables to perform tabulations by. All combinations of the variables
#'   specified here appear in results. Default is `dplyr::group_vars(data)`.
#' @inheritParams ard_categorical
#'
#' @return an ARD data frame of class 'card'
#' @name ard_hierarchical
#'
#' @examples
#' ard_hierarchical(
#'   data = ADAE,
#'   variables = c(AESOC, AETERM),
#'   by = c(TRTA, AESEV),
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM)
#' )
#'
#' ard_hierarchical_count(
#'   data = ADAE,
#'   variables = c(AESOC, AETERM),
#'   by = TRTA
#' )
NULL

#' @rdname ard_hierarchical
#' @export
ard_hierarchical <- function(data,
                             variables,
                             by = dplyr::group_vars(data),
                             statistics = everything() ~ categorical_variable_summary_fns(),
                             denominator = NULL, fmt_fn = NULL,
                             stat_labels = everything() ~ default_stat_labels()) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_data_frame(x = data)

  # process arguments ----------------------------------------------------------
  process_selectors(
    data,
    variables = {{ variables }},
    by = {{ by }}
  )
  data <- dplyr::ungroup(data)

  # return empty tibble if no variables selected -------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble())
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
      statistics = statistics,
      denominator = denominator,
      fmt_fn = fmt_fn,
      stat_labels = stat_labels
    )

  # renaming columns -----------------------------------------------------------
  df_result <- .rename_last_group_as_variable(df_result)

  # return ard -----------------------------------------------------------------
  df_result |>
    dplyr::mutate(context = "hierarchical")
}

#' @rdname ard_hierarchical
#' @export
ard_hierarchical_count <- function(data,
                                   variables,
                                   by = dplyr::group_vars(data),
                                   fmt_fn = NULL,
                                   stat_labels = everything() ~ default_stat_labels()) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_data_frame(x = data)

  # process arguments ----------------------------------------------------------
  process_selectors(
    data,
    variables = {{ variables }},
    by = {{ by }}
  )

  # return empty tibble if no variables selected -------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }

  # add dummy variable for counting --------------------------------------------
  data[["...ard_dummy_for_counting..."]] <- 1L

  # perform tabulations --------------------------------------------------------
  seq_along(variables) |>
    rev() |>
    lapply(
      function(i) {
        ard_categorical(
          data = data,
          variables = "...ard_dummy_for_counting...",
          by = all_of(by),
          strata = all_of(variables[seq_len(i)]),
          statistics = everything() ~ categorical_variable_summary_fns("n"),
          fmt_fn = fmt_fn,
          stat_labels = stat_labels
        ) |>
          .rename_last_group_as_variable()
      }
    ) |>
    bind_ard() |>
    dplyr::mutate(context = "hierarchical_count") %>%
    {structure(., class = unique(c("card", class(.))))} # styler: off
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