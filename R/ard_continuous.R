#' Continuous ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for simple continuous summary statistics.
#'
#' @param data a data frame
#' @param variables columns to include in summaries.
#' @param by,strata columns to by/stratified by for summary statistic
#' calculation. Arguments are similar, but with an important distinction:
#'
#' `by`: results are calculated by **all combinations** of the columns specified,
#' including unobserved combinations and unobserved factor levels.
#'
#' `strata`: results are calculated by **all _observed_ combinations** of the
#' columns specified.
#'
#' Arguments may be used in conjunction with one another.
#'
#' @param statistics a named list of functions, a list of formulas,
#' or a single formula where the list element is a function (or the RHS of
#' a formula),
#' e.g. `list(mpg = list(mean = \(x) mean(x)))`.
#'
#' The value assigned to each variable must also be a named list, where the names
#' are used to reference a function and the element is the function object.
#' Typically, this function will return a scalar statistic, but a function that
#' returns a named list of results is also acceptable, e.g.
#' `list(conf.low = -1, conf.high = 1)`. However, when errors occur, the messaging
#' will be less clear in this setting.
#'
#' See the [`selecting_syntax`] help file for details.
#'
#' @param stat_labels a named list, a list of formulas, or a single formula where
#'   the list element is either a named list or a list of formulas defining the
#'   statistic labels, e.g. `everything() ~ list(mean = "Mean", sd = "SD")` or
#'   `everything() ~ list(mean ~ "Mean", sd ~ "SD")`.
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
                           strata = NULL,
                           statistics = everything() ~ continuous_variable_summary_fns(),
                           stat_labels = everything() ~ default_stat_labels()
                           ) {

  # check inputs ---------------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variables, "variables")
  check_class_data_frame(data = data)
  check_class(class = c("list", "formula"), statistics = statistics, allow_null = TRUE)
  check_class(class = c("list", "formula"), stat_labels = stat_labels, allow_null = TRUE)

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  process_selectors(data, variables = {{variables}}, by = {{by}}, strata = {{strata}})
  process_formula_selectors(data = data[variables], statistics = statistics)
  process_formula_selectors(data = data[variables], stat_labels = stat_labels)

  check_list_elements(
    statistics = function(x) is.list(x) && rlang::is_named(x) && every(x, is.function),
    error_msg =
      list(statistics =
             c("Error in the argument {.arg {arg_name}} for variable {.val {variable}}.",
               "i" = "Value must be a named list of functions."))
  )

  # return empty tibble if no variables selected -------------------------------
  if (rlang::is_empty(variables)) return(dplyr::tibble())

  # setting default statistics -------------------------------------------------
  statistics <-
    variables |>
    lapply(function(x) statistics[[x]] %||% continuous_variable_summary_fns()) |>
    stats::setNames(nm = variables)

  # setting default stat labels -------------------------------------------------
  stat_labels <-
    variables |>
    lapply(function(x) stat_labels[[x]] %||% default_stat_labels()) |>
    stats::setNames(nm = variables)

  # final processing of stat labels -------------------------------------------------
  df_stat_labels <- process_stat_labels(stat_labels = stat_labels, statistics = statistics)

  # calculate statistics -------------------------------------------------------
  df_nested <-
    data |>
    nest_for_ard(
      by = by,
      strata = strata,
      key = "...ard_nested_data..."
    )

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
      df_stat_labels,
      by = c("variable", "stat_name")
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

process_stat_labels <- function(stat_labels, statistics){

  # create the tibble of stat names and labels 1 variable at a time
   # both the stat_labels and statistics are a named (variable-level) list of stat info
  stat_labels <- map2(stat_labels, statistics, function(x,y){

    # handle the named list or formula & create tibble
       # note:
       # - when x is a named list, if a labeled stat is not found in the statistics it is ignored
       # - if x is a formula, if a labeled stat is not found is the statistics an error is thrown
    compute_formula_selector(data=y, x=x) %>%
      {dplyr::tibble(
        stat_name = names(.),
        stat_label = unlist(.) |> unname()
      )}
  })

  # stack result
  stat_labels |>
    dplyr::bind_rows(.id = "variable")

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
