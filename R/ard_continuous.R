#' Continuous ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for simple continuous summary statistics.
#'
#' @param data a data frame
#' @param by columns to compute statistics by. Default are the columns
#' returned by `dplyr::group_vars(data)`.
#' @param statistics a named list of functions, a list of formulas,
#' or a single formula where the list element is a function (or the RHS of
#' a formula),
#' e.g. `list(mpg = list(mean = \(x) mean(x, na.rm = TRUE)))`
#' @param variables columns to include in summaries.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard_continuous(ADSL, by = "ARM", variables = "AGE") |>
#'   flatten_ard()

ard_continuous <- function(data,
                           variables,
                           by = NULL,
                           statistics = everything() ~ continuous_variable_summary_fns()) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variables, "variables")
  check_class_data_frame(data = data)
  check_class(class = c("list", "formula"), statistics = statistics, allow_null = TRUE)

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  process_selectors(data, variables = {{variables}}, by = {{by}})
  process_formula_selectors(data = data[variables], statistics = statistics)

  # setting default statistics -------------------------------------------------
  statistics <-
    variables |>
    lapply(function(x) statistics[[x]] %||% continuous_variable_summary_fns()) |>
    stats::setNames(nm = variables)

  # calculate statistics -------------------------------------------------------
  df_nested <-
    data |>
    .ard_nest(
      by = by,
      key = "...ard_nested_data..."
    )

  # add columns for the by variables
  if (!rlang::is_empty(by)) {
    df_nested <-
      df_nested |>
      # setting column names for stratum levels
      dplyr::mutate(!!!(as.list(by) |> stats::setNames(paste0("group", seq_along(by)))), .before = 0L) |>
      dplyr::rename(!!!(as.list(by) |> stats::setNames(paste0("group", seq_along(by), "_level"))))
  }

  # calculate statistics indicated by user in statistics argument
  df_nested <-
    .calculate_stats_as_ard(
      df_nested = df_nested,
      variables = variables,
      statistics = statistics,
      new_col_name = "..ard_all_stats..",
      omit_na = TRUE
    )

  # add variable-level stats like length of vector, proportion missing, etc.
  if (!isTRUE(getOption("cards.ard_continuous.suppress-variable-level-stats"))) {
    variable_level_stats <-
      compute_formula_selector(
        data = data[variables],
        x =
          ~list(var_level = function(x) dplyr::tibble(N_obs = length(x),
                                                      N_miss = sum(is.na(x)),
                                                      N_nonmiss = N_obs - N_miss,
                                                      p_miss = N_miss / N_obs,
                                                      p_nonmiss = N_nonmiss / N_obs))
      )
    df_nested_variable_level_stats <-
      .calculate_stats_as_ard(
        df_nested = df_nested,
        variables = variables,
        statistics = variable_level_stats,
        new_col_name = "..ard_all_stats..",
        omit_na = FALSE
      )
  }
  else df_nested_variable_level_stats <- data.frame()



  # unnest results and add default function labels and formatters
  df_results <-
    dplyr::bind_rows(df_nested, df_nested_variable_level_stats) |>
    dplyr::select(all_ard_groups(), "..ard_all_stats..") |>
    tidyr::unnest(cols = "..ard_all_stats..") |>
    dplyr::left_join(
      .default_statistic_labels(),
      by = "stat_name"
    ) |>
    dplyr::left_join(
      .default_statistic_formatters(),
      by = "stat_name"
    ) |>
    dplyr::mutate(
      stat_label = ifelse(is.na(.data$stat_label), .data$stat_name, .data$stat_label),
      statistic_fmt_fn =
        .data$statistic_fmt_fn |>
        lapply(function(fn) fn %||% function(x) format(round(x, digits = 0), nsmall = 0))
    )

  # add meta data and class
  df_results |>
    dplyr::mutate(context = "continuous") |>
    dplyr::arrange(dplyr::across(all_ard_groups())) |>
    tidy_ard_column_order() %>%
    structure(., class = c("card", class(.)))
}


.calculate_stats_as_ard <- function(df_nested, variables, statistics,
                                    new_col_name = "..ard_all_stats..",
                                    omit_na = TRUE) {
  pre_process_fun <- if (isTRUE(omit_na)) stats::na.omit else identity

  df_nested[[new_col_name]] <-
    map(
      df_nested[["...ard_nested_data..."]],
      function(nested_data) {
        map(
          variables,
          function(variable) {
            map2(
              statistics[[variable]], names(statistics[[variable]]),
              function(fun, fun_name) {
                .lst_results_as_df(
                  x = # calculate results, and place in tibble
                    eval_capture_conditions(
                      do.call(fun, args = list(pre_process_fun(nested_data[[variable]])))
                    ),
                  variable = variable,
                  fun_name = fun_name
                )
              }
            ) |>
              unname()
          }
        ) |>
          dplyr::bind_rows()
      }
    )

  df_nested
}


.default_statistic_labels <- function() {
  list(
    mean = "Mean",
    sd = "SD",
    var = "Variance",
    median = "Median",
    p25 = "25th Percentile",
    p75 = "75th Percentile",
    min = "Min",
    max = "Max",
    n = "n",
    N = "N",
    length = "Vector Length",
    p = "%",
    p_cell = "%"
  ) %>%
    {dplyr::tibble(
      stat_name = names(.),
      stat_label = unlist(.) |> unname()
    )}
}

.lst_results_as_df <- function(x, variable, fun_name) {
  # unnesting results if needed
  if (.is_named_list(x$result, allow_df = TRUE)) {
    if (is.data.frame(x$result)) x$result <- unclass(x$result)
    df_ard <-
      dplyr::tibble(
        stat_name = names(x$result),
        result = unname(x$result),
        warning = list(x$warning),
        error = list(x$error)
      )
  }
  # if result is not a nested list, return a single row tibble
  else {
    df_ard <-
      map(x, list) |>
      dplyr::as_tibble() |>
      dplyr::mutate(stat_name = .env$fun_name)
  }

  df_ard |>
    dplyr::mutate(variable = .env$variable) |>
    dplyr::rename(statistic = "result")
}


# the global default is to round the statistic to one decimal place.
# for all other rounding, the default function must be listed below
.default_statistic_formatters <- function() {
  list(
    n = function(x) format(round(x, digits = 0), nsmall = 0),
    N_miss = function(x) format(round(x, digits = 0), nsmall = 0),
    length = function(x) format(round(x, digits = 0), nsmall = 0),
    p = function(x) format(round(x * 100, digits = 1), nsmall = 1),
    p_cell = function(x) format(round(x * 100, digits = 1), nsmall = 1)
  ) %>%
    {dplyr::tibble(
      stat_name = names(.),
      statistic_fmt_fn = unname(.)
    )}
}
