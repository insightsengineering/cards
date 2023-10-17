#' Categorical ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for categorical summary statistics.
#'
#' @param data a data frame
#' @param by columns to compute statistics by. Default are the columns
#' returned by `dplyr::group_vars(data)`.
#' @param variables columns to include in summaries. Default is `everything()`.
#' @param denominator Specify this *optional* argument to change the denominator,
#' e.g. the `"N"` statistic. Default is `NULL`. See below for details.
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
#' specified in `ard_categorical(by=)`.
#' The updated `N` and `length` elements will be updated to be calculated as
#' the number of rows in each combination of the `by` variables.
#'
#' Take an example where we need to update the denominator to be subjects enrolled
#' in a trial, e.g. tabulating the number of AEs that occurred within an SOC
#' where some subjects may not have experienced an AE and would not be represented
#' in the ADAE data set. All patients appear in ADSL, however.
#'
#' ```{r}
#' ard_categorical(
#'   data =
#'     ADAE |>
#'       dplyr::left_join(ADSL[c("USUBJID", "ARM")], by = "USUBJID") |>
#'       dplyr::filter(AOCCSFL %in% "Y"),
#'   by = "ARM",
#'   variables = "AESOC",
#'   denominator = ADSL
#' ) |>
#'   flatten_ard()
#' ```
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
ard_categorical <- function(data, variables, by = NULL, denominator = NULL) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variables, "variables")
  check_class_data_frame(data = data)

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  process_selectors(data, variables = {{ variables }}, by = {{ by }})

  # check inputs ---------------------------------------------------------------
  if (!is.null(denominator)) {
    check_class_data_frame(denominator = denominator)
    check_columns_in_data_frame(
      denominator, columns = by,
      msg = "Columns {.val {missing_cols}} must appear in {.arg denominator}.")
  }

  # calculating summary stats --------------------------------------------------
  withr::with_options(
    new = list("cards.ard_continuous.suppress-variable-level-stats" = !is.null(denominator)),
    df_result <-
      ard_continuous(
        data = # creating a factor, so unobserved levels appear in tabulation
          data |>
          dplyr::mutate(across(all_of(variables),
                               ~factor(.x, levels = .unique_and_sorted(.x)))),
        variables = all_of(variables),
        by = all_of(by),
        statistics = ~list(table = function(x) table(x), N = function(x) length(x))
      )
  )

  # if the denominator argument is supplied, then re-calculate the N statistic
  if (!is.null(denominator)) {
    df_variables_only <-
      matrix(TRUE, nrow = nrow(denominator), ncol = length(variables)) |>
      data.frame() |>
      stats::setNames(variables)

    withr::with_options(
      new = list("cards.ard_continuous.suppress-variable-level-stats" = TRUE),
      df_result_N <-
        ard_continuous(
          data = denominator[by] |> dplyr::bind_cols(df_variables_only),
          variables = all_of(variables),
          by = all_of(by),
          statistics = ~list(N = function(x) length(x))
        )
    )

    df_result <- suppressMessages(bind_ard(df_result, df_result_N, .update = TRUE))
  }

  # process the table() results and add to the ARD data frame
  df_results_n <-
    df_result |>
    dplyr::filter(.data$stat_label %in% "table") |>
    dplyr::mutate(
      variable_level = map(.data$variable, ~.unique_and_sorted(data[[.x]]) |> as.list()),
      statistic = map(.data$statistic, ~as.integer(.x) |> as.list()),
      stat_name = "n",
      stat_label = "n"
    ) |>
    tidyr::unnest(cols = c("variable_level", "statistic"))

  df_results_p <-
    df_results_n %>%
    {suppressMessages(dplyr::left_join(
      .,
      df_result |>
        dplyr::filter(.data$stat_name %in% "N") |>
        dplyr::select(all_ard_groups(), "variable", ..N.. = "statistic")
    ))} |>
    dplyr::mutate(
      statistic = map2(.data$statistic, .data$..N.., ~.x / .y),
      stat_name = "p",
      stat_label = "%",
      statistic_fmt_fn = list(function(x) format(round(x * 100, digits = 1), nsmall = 1))
    ) |>
    dplyr::select(-"..N..")

  # combine all results
  dplyr::bind_rows(
    df_result |> dplyr::filter(!.data$stat_name %in% "table"),
    df_results_n,
    df_results_p
  ) |>
    dplyr::rows_update(
      .default_statistic_labels(),
      by = "stat_name",
      unmatched = "ignore"
    ) |>
    dplyr::arrange(dplyr::across(c(all_ard_groups(), all_ard_variables()))) |>
    dplyr::mutate(context = "categorical") |>
    tidy_ard_column_order() %>%
    structure(., class = c("card", class(.)))
}



# .update_percent_statistic <- function(x) {
#   x |>
#     dplyr::group_by(
#       dplyr::pick(dplyr::any_of("variable")),
#       dplyr::pick(dplyr::matches("^group[0-9]+$")),
#       dplyr::pick(dplyr::matches("^group[0-9]+_level$"))
#     ) |>
#     dplyr::group_map(
#       ~ dplyr::filter(.x, .data$stat_name %in% "n") |>
#         dplyr::mutate(
#           stat_name = "p",
#           statistic =
#             (unlist(.data$statistic) / unlist(.x[.x$stat_name %in% "N", "statistic"])) |>
#             as.list()
#         ),
#       .keep = TRUE
#     ) %>%
#     {dplyr::bind_rows(
#       x |>
#         # remove the old percent calculation based on the typical denominator
#         dplyr::filter(!.data$stat_name %in% "p"),
#       !!!.
#     )} |>
#     # the length stat becomes much more difficult to interpret in this situation, so removing
#     dplyr::filter(!.data$stat_name %in% "length")
# }


