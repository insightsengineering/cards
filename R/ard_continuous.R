#' Continuous ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for simple continuous summary statistics.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to include in summaries
#' @param by,strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to by/stratified by for summary statistic
#'   calculation. Arguments are similar, but with an important distinction:
#'
#'   `by`: results are calculated by **all combinations** of the columns specified,
#'      including unobserved combinations and unobserved factor levels.
#'
#'   `strata`: results are calculated by **all _observed_ combinations** of the
#'     columns specified.
#'
#'   Arguments may be used in conjunction with one another.
#'
#' @param statistics ([`formula-list-selector`][selecting_syntax])\cr
#'   a named list, a list of formulas,
#'   or a single formula where the list element is a named list of functions
#'   (or the RHS of a formula),
#'   e.g. `list(mpg = list(mean = \(x) mean(x)))`.
#'
#'   The value assigned to each variable must also be a named list, where the names
#'   are used to reference a function and the element is the function object.
#'   Typically, this function will return a scalar statistic, but a function that
#'   returns a named list of results is also acceptable, e.g.
#'   `list(conf.low = -1, conf.high = 1)`. However, when errors occur, the messaging
#'   will be less clear in this setting.
#'
#' @param fmt_fn ([`formula-list-selector`][selecting_syntax])\cr
#'   a named list, a list of formulas,
#'   or a single formula where the list element is a named list of functions
#'   (or the RHS of a formula),
#'   e.g. `list(mpg = list(mean = \(x) round(x, digits = 2) |> as.character))`.
#'
#' @param stat_labels ([`formula-list-selector`][selecting_syntax])\cr
#'   a named list, a list of formulas, or a single formula where
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
                           fmt_fn = everything() ~ default_fmt_fns(),
                           stat_labels = everything() ~ default_stat_labels()) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_class_data_frame(data = data)
  check_class(class = c("list", "formula"), statistics = statistics, allow_null = TRUE)
  check_class(class = c("list", "formula"), stat_labels = stat_labels, allow_null = TRUE)
  check_class(class = c("list", "formula"), fmt_fn = fmt_fn, allow_null = TRUE)

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  process_selectors(data, variables = {{variables}}, by = {{by}}, strata = {{strata}})

  process_formula_selectors(
    data = data[variables],
    statistics = statistics,
    fmt_fn = fmt_fn,
    stat_labels = stat_labels
  )
  fill_formula_selectors(
    data = data[variables],
    statistics = formals(cards::ard_continuous)[["statistics"]] |> eval(),
    fmt_fn = formals(cards::ard_continuous)[["fmt_fn"]] |> eval(),
    stat_labels =  formals(cards::ard_continuous)[["stat_labels"]] |> eval()
  )

  check_list_elements(
    statistics = function(x) is.list(x) && rlang::is_named(x) && every(x, is.function),
    error_msg =
      list(statistics =
             c("Error in the argument {.arg {arg_name}} for variable {.val {variable}}.",
               "i" = "Value must be a named list of functions."))
  )

  # return empty tibble if no variables selected -------------------------------
  if (rlang::is_empty(variables)) return(dplyr::tibble())

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

  # unnest results
  df_results <-
    df_nested |>
    dplyr::select(all_ard_groups(), "..ard_all_stats..") |>
    tidyr::unnest(cols = "..ard_all_stats..")

  # add default function labels and formatters
  df_results <-
    df_nested |>
    dplyr::select(all_ard_groups(), "..ard_all_stats..") |>
    tidyr::unnest(cols = "..ard_all_stats..")

  # final processing of fmt_fn -------------------------------------------------
  df_fmt_fn <- .process_stat_arg(data = df_results,
                                 stat_arg_list = fmt_fn,
                                 col_name = "statistic_fmt_fn")

  # final processing of stat labels -------------------------------------------------
  df_stat_labels <- .process_stat_arg(data = df_results,
                                      stat_arg_list = stat_labels,
                                      col_name = "stat_label") |>
  tidyr::unnest("stat_label")

  df_results_fmt <-
    df_results |>
    dplyr::left_join(
      df_stat_labels,
      by = c("variable", "stat_name")
    ) |>
    dplyr::left_join(
      df_fmt_fn,
      by = c("variable", "stat_name")
    ) |>
    dplyr::mutate(
      stat_label = ifelse(is.na(.data$stat_label), .data$stat_name, .data$stat_label),
      statistic_fmt_fn =
        .data$statistic_fmt_fn |>
        lapply(function(fn) fn %||% function(x) format(round5(x, digits = 1), nsmall = 1))
    )

  # add meta data and class
  df_results_fmt |>
    dplyr::mutate(context = "continuous") |>
    dplyr::arrange(dplyr::across(all_ard_groups())) |>
    tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))}
}

#' Calculate Continuous Statistics
#'
#' Calculate statistics and return in an ARD format
#'
#' @param df_nested a nested data frame
#' @param variables character vector of variables
#' @param statistics named list of statistical functions
#' @param new_col_name string of new column name
#' @param omit_na logical indicating whether to omit NA values before calculating
#' statistics. Default is TRUE
#'
#' @keywords internal
#' @return data frame
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

#' Process Statistic Labels
#'
#' @param data ARD data frame containing variable and statistics columns
#' @param stat_arg_list named list
#' @param col_name column name in the ARD to make for the stat argument
#' @return named list
#' @keywords internal
#' @examples
#' ard <- ard_categorical(ADSL, variables = "AGE")
#' stat_arg_list <- list(AGE = list(c("N", "n") ~ "{n} / {N}"))
#' cards:::.process_stat_arg(data = ard, stat_arg_list = stat_arg_list, col_name = "stat_label")
.process_stat_arg <- function(data, stat_arg_list, col_name){

  # create the tibble of stat names and arg values 1 variable at a time
  args_tbl <-
    imap(
      stat_arg_list,
      function(x, y) {
        # get a named vector of stats to evaluate on
        stats <- data |>
          dplyr::filter(.data$variable == y) |>
          dplyr::pull(.data$stat_name) %>%
          set_names(., .)

        # handle the named list or formula & create tibble
        #   simply ignore any formats for stats not found in data,
        #   if multiple specified for 1 stat, keep the first
        stats_df <- compute_formula_selector(data=stats, x=x, strict=FALSE) %>%
          {dplyr::tibble(
            stat_name = names(.),
            !!col_name := unname(.)
          )} |>
          dplyr::group_by(.data$stat_name) |>
          dplyr::slice(1) |>
          dplyr::ungroup()

        if (!is.list(stats_df[[col_name]])){
          stats_df[[col_name]] <- as.list(stats_df[[col_name]])
        }

        stats_df
      }
    )

  # stack result
  args_tbl |>
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




#' Default formatting functions
#'
#' Global default is to round the statistic to one decimal place for
#' all other rounding, the default function must be listed below
#'
#' @return named list of functions
#' @keywords internal
#'
#' @examples
#' cards:::.default_statistic_formatters()
.default_statistic_formatters <- function() {
  list(
    n = function(x) format(round5(x, digits = 0), nsmall = 0),
    N_miss = function(x) format(round5(x, digits = 0), nsmall = 0),
    length = function(x) format(round5(x, digits = 0), nsmall = 0),
    p = function(x) format(round5(x * 100, digits = 1), nsmall = 1),
    p_cell = function(x) format(round5(x * 100, digits = 1), nsmall = 1)
  ) %>%
    {dplyr::tibble(
      stat_name = names(.),
      statistic_fmt_fn = unname(.)
    )}
}
