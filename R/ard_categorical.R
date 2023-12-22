#' Categorical ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for categorical summary statistics.
#'
#' @param data (`data.frame`)\cr
#' a data frame
#' @param by,strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to by/stratified by for tabulation.
#'   Arguments are similar, but with an important distinction:
#'
#'   `by`: results are tabulated by **all combinations** of the columns specified,
#'      including unobserved combinations and unobserved factor levels.
#'
#'   `strata`: results are tabulated by **all _observed_ combinations** of the
#'     columns specified.
#'
#'   Arguments may be used in conjunction with one another.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to include in summaries. Default is `everything()`.
#' @param denominator (`data.frame`)\cr
#'   Specify this *optional* argument to change the denominator,
#'   e.g. the `"N"` statistic. Default is `NULL`. See below for details.
#' @param statistics ([`formula-list-selector`][selecting_syntax])\cr
#'   a named list, a list of formulas,
#'   or a single formula where the list element is a named list of functions
#'   (or the RHS of a formula),
#'   e.g. `list(mpg = categorical_variable_summary_fns())`.
#' @param stat_labels ([`formula-list-selector`][selecting_syntax])\cr
#'   a named list, a list of formulas, or a single formula where
#'   the list element is either a named list or a list of formulas defining the
#'   statistic labels, e.g. `everything() ~ list(n = "n", p = "pct")` or
#'   `everything() ~ list(n ~ "n", p ~ "pct")`.
#' @inheritParams ard_continuous
#'
#' @section Denominators:
#' By default, the `ard_categorical()` function returns the statistics `"n"` and `"N"`,
#' where little `"n"` are the counts for the variable levels, and `"N"` is
#' the number of non-missing observations. The default calculation for the
#' percentage is merely `p = n/N`.
#'
#' However, it is sometimes necessary to provide a different `"N"` to use
#' as the denominator in this calculation. For example, in a calculation
#' of the rates of various observed adverse events, you may need to update the
#' denominator to the number of enrolled subjects.
#'
#' In such cases, use the `denominator` argument to specify a new definition
#' of `"N"`, and subsequently `"p"`.
#' The argument expects a data frame, and the data frame must include the columns
#' specified in `ard_categorical(by=)` (strata columns are not considered).
#' The updated `N` and `length` elements will be updated to be calculated as
#' the number of rows in each combination of the `by` variables.
#'
#' @return a data frame
#' @name ard_categorical
#' @export
#'
#' @examples
#' ard_categorical(ADSL, by = "ARM", variables = "AGEGR1") |>
#'   flatten_ard()
NULL

#' @rdname ard_categorical
#' @export
ard_categorical <- function(data, variables, by = NULL, strata = NULL,
                            statistics = everything() ~ categorical_variable_summary_fns(),
                            denominator = NULL,
                            fmt_fn = NULL,
                            stat_labels = everything() ~ default_stat_labels()) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_class_data_frame(data = data)
  check_class(class = c("list", "formula"), stat_labels = stat_labels, allow_null = TRUE)

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  process_selectors(
    data,
    variables = {{ variables }},
    by = {{ by }},
    strata = {{ strata }}
  )

  process_formula_selectors(
    data = data[variables],
    statistics = statistics,
    stat_labels = stat_labels,
    fmt_fn = fmt_fn
  )
  fill_formula_selectors(
    data = data[variables],
    statistics = formals(cards::ard_continuous)[["statistics"]] |> eval()
  )

  # check inputs ---------------------------------------------------------------
  if (!is.null(denominator)) {
    check_class_data_frame(denominator = denominator)
    check_columns_in_data_frame(
      denominator, columns = by,
      msg = "Columns {.val {missing_cols}} must appear in {.arg denominator}.")
  }

  # return empty tibble if no variables selected -------------------------------
  if (rlang::is_empty(variables)) return(dplyr::tibble())

  # calculating summary stats --------------------------------------------------
  df_result <-
    ard_continuous(
      data = # creating a factor, so unobserved levels appear in tabulation
        data |>
        dplyr::mutate(across(all_of(variables),
                             ~factor(.x, levels = .unique_and_sorted(.x)))),
      variables = all_of(variables),
      by = all_of(by),
      strata = all_of(strata),
      statistics = statistics,
      fmt_fn = NULL,
      stat_labels = NULL
    ) |>
    dplyr::select(-"statistic_fmt_fn", -"stat_label")

  # if the denominator argument is supplied, then re-calculate the N statistic -
  if (!is.null(denominator))
    df_result <-
    .ard_categorical_recalc_N(df_result, denominator, by, variables)

  # process the table() results and add to the ARD data frame ------------------
  df_result_final <- .unnest_table_object(df_result, data)

  # final processing of fmt_fn -------------------------------------------------
  df_result_final <-
    .process_nested_list_as_df(
      x = df_result_final,
      arg = fmt_fn,
      new_column = "statistic_fmt_fn"
    ) |>
    .default_fmt_fn()

  # final processing of stat labels --------------------------------------------
  df_result_final <-
    .process_nested_list_as_df(
      x = df_result_final,
      arg = stat_labels,
      new_column = "stat_label",
      unlist = TRUE
    ) |>
    dplyr::mutate(
      stat_label =
        map2_chr(
          .data$stat_label, .data$stat_name,
          function(stat_label, stat_name) dplyr::coalesce(stat_label, default_stat_labels()[[stat_name]], stat_name)
        )
    )

  # merge in stat labels and format ARD for return -----------------------------
  df_result_final |>
    dplyr::arrange(dplyr::across(c(all_ard_groups(), all_ard_variables()))) |>
    dplyr::mutate(context = "categorical") |>
    tidy_ard_column_order() %>%
    {structure(., class = unique(c("card", class(.))))}
}


#' Unnest `table()` to ARD Structure
#'
#' This function takes the 'statistic' returned from the `table()` function,
#' and unnests it to the ARD format including the `"variable_level"` column
#' and the scalar statistics (i.e. `"n"` and `"p"`).
#'
#' @param df_result an ARD data frame
#' @param data the data frame used to construct the ARD data frame. This is used
#' to extract the variable levels and add to the unnested data frame.
#'
#' @return an ARD data frame
#' @keywords internal
#'
#' @examples
#' ard_continuous(
#'   mtcars,
#'   variables = cyl,
#'   statistics = cyl ~ list(table = function(x) table(x))
#' ) |>
#'   cards:::.unnest_table_object(data = mtcars)
.unnest_table_object <- function(df_result, data) {
  # convert table() results to counts (n)
  df_results_tables <-
    df_result |>
    dplyr::filter(map_lgl(.data$statistic, ~inherits(.x, "table"))) |>
    dplyr::mutate(
      variable_level = map(.data$variable, ~.unique_and_sorted(data[[.x]]) |> as.list()),
      statistic = map(.data$statistic, ~as.list(.x))
    ) |>
    tidyr::unnest(cols = c("variable_level", "statistic"))

  # append and percent results to primary results
  dplyr::bind_rows(
    df_result |> dplyr::filter(!map_lgl(.data$statistic, ~inherits(.x, "table"))),
    df_results_tables
  )
}

# when the denominator argument is passed, recalculate the N statistic
#' Recalculate N and p
#'
#' If a user passes `ard_categorical(denominator)` the N and p need to be adjusted
#' to match the data frame in `denominator`.
#'
#' @param df_result an ARD data frame
#' @param denominator a data frame
#' @param by character vector of by variables passed in `ard_categorical(by)`
#' @param variables vector string of the variables passed in `ard_categorical(variables)`
#'
#' @return an ARD data frame
#'
#' @keywords internal
#' @examples
#' ard_categorical(
#'   mtcars,
#'   variables = cyl
#' ) |>
#'   cards:::.ard_categorical_recalc_N(
#'     denominator = rep(list(mtcars), 10) |> dplyr::bind_rows(),
#'     by = NULL,
#'     variables = "cyl"
#'   ) |>
#'   flatten_ard()
.ard_categorical_recalc_N <- function(df_result, denominator, by, variables) {
  # create a data set for the calculation
  df_variables_only <-
    matrix(TRUE, nrow = nrow(denominator), ncol = length(variables)) |>
    data.frame() |>
    stats::setNames(variables)

  # calculate big N on denominator data set
  df_result_denom_N <-
    ard_continuous(
      data = denominator[by] |> dplyr::bind_cols(df_variables_only),
      variables = all_of(variables),
      by = all_of(by),
      statistics = ~categorical_variable_summary_fns("N")
    ) |>
    dplyr::select(-"stat_label", -"statistic_fmt_fn")

  # recalculating the percentages using the new big N
  df_result_denom_p <-
    df_result |>
    dplyr::filter(.data$stat_name %in% "n") |>
    dplyr::mutate(
      stat_name = "p"
    ) %>%
    # merging on all common columns and suppressing the merged variable note
    {suppressMessages(dplyr::left_join(
      .,
      df_result_denom_N |>
        dplyr::mutate(big_N = unlist(.data$statistic)) |>
        dplyr::select(all_ard_groups(), all_ard_variables(), "big_N")
    ))} |>
    dplyr::mutate(
      statistic = map2(.data$statistic, .data$big_N, function(x, y) x / y)
    ) |>
    dplyr::select(-"big_N")

  # stacking results together
  df_result |>
    dplyr::filter(!.data$stat_name %in% c("p", "N")) |>
    dplyr::bind_rows(
      df_result_denom_N,
      df_result_denom_p
    )
}
