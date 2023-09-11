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
#'   by = ARM,
#'   variables = "AESOC",
#'   denominator = ADSL
#' ) |>
#'   flatten_ard()
#' ```
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard_categorical(ADSL, by = "ARM", variables = "AGEGR1") |>
#'   flatten_ard()
ard_categorical <- function(data, variables, by = NULL, denominator = NULL) {
  # process arguments -----------------------------------------------------------
  by <- dplyr::select(data, {{ by }}) |> colnames()
  variables <- dplyr::select(data, {{ variables }}) |> colnames() |> setdiff(by)
  data <- dplyr::ungroup(data)

  # check inputs (will make this more robust later) ----------------------------
  if (!is.null(denominator)){
    if (!is.data.frame(denominator))
      cli::cli_abort("The {.code denominator} argument must be class {.cls data.frame}.")
    if (!rlang::is_empty(setdiff(by, names(denominator))))
      cli::cli_abort("The {.code denominator} data frame must contain columns {.val {by}}.")
  }

  # calculating summary stats --------------------------------------------------
  # first, calculate the variable level statistics (e.g. N, length)
  df_ard_variables <-
    ard_continuous(
      data =
        switch(
          is.null(denominator) |> as.character(),
          "TRUE" = data,
          "FALSE" =
            denominator |>
            dplyr::select(all_of(by)) |>
            dplyr::mutate(!!!(rep_len(list(1L), length(variables)) |> stats::setNames(variables)))
        ),
      variables = variables,
      by = by,
      statistics =
        variables |>
        lapply(function(x) .default_continuous_statistics()[c("N", "length")]) |>
        stats::setNames(nm = variables)
    )

  # second, tabulate variable
  df_ard_tablulation <-
    lapply(
      X = variables,
      FUN = function(v) {
        ard_continuous(
          data = data |> dplyr::select(all_of(c(by, v))) |> tidyr::drop_na(),
          variables = dplyr::all_of(v),
          by = !!all_of(by),
          statistics =
            list(
              table = function(x) {
                dplyr::tibble(
                  variable_level = # referencing `data` to get all observed levels in the full data set
                    rlang::inject(!!.unique_and_sorted(data[[v]])),
                  n = # creating a factor, so unobserved levels appear in tabulation
                    factor(x, levels = rlang::inject(!!.unique_and_sorted(data[[v]]))) |>  table() |> as.integer(),
                  p = .data$n / sum(.data$n)
                )
              }
            ) |>
            list() |>
            stats::setNames(nm = v)
        ) |>
          dplyr::select(-"stat_name") |>
          tidyr::unnest(cols = "statistic") |>
          dplyr::mutate(
            dplyr::across(c("variable_level", "n", "p"), .fns = as.list)
          ) |>
          tidyr::pivot_longer(
            cols = c("n", "p"),
            names_to = "stat_name",
            values_to = "statistic"
          )
      }
    ) |>
    dplyr::bind_rows()

  # bind data frames with stats, and return to user ----------------------------
  ard_final <- dplyr::bind_rows(df_ard_tablulation, df_ard_variables)
  # if a custom function for the denom was provided, udpate the percent statistics using the custom N calculation
  if (!rlang::is_empty(denominator))
    ard_final <- .update_percent_statistic(ard_final)

  ard_final |>
    dplyr::rows_update(
      .default_statistic_labels(),
      by = "stat_name",
      unmatched = "ignore"
    ) |>
    dplyr::mutate(context = "categorical") %>%
    structure(., class = c("card", class(.)))
}



.update_percent_statistic <- function(x) {
  x |>
    dplyr::group_by(
      dplyr::pick(dplyr::any_of("variable")),
      dplyr::pick(dplyr::matches("^group[0-9]+$")),
      dplyr::pick(dplyr::matches("^group[0-9]+_level$"))
    ) |>
    dplyr::group_map(
      ~ dplyr::filter(.x, .data$stat_name %in% "n") |>
        dplyr::mutate(
          stat_name = "p",
          statistic =
            (unlist(.data$statistic) / unlist(.x[.x$stat_name %in% "N", "statistic"])) |>
            as.list()
        ),
      .keep = TRUE
    ) %>%
    {dplyr::bind_rows(
      x |>
        # remove the old percent calculation based on the typical denominator
        dplyr::filter(!.data$stat_name %in% "p"),
      !!!.
    )}
}


