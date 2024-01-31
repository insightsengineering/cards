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
#' @param statistics ([`formula-list-selector`][syntax])\cr
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
#' @param fmt_fn ([`formula-list-selector`][syntax])\cr
#'   a named list, a list of formulas,
#'   or a single formula where the list element is a named list of functions
#'   (or the RHS of a formula),
#'   e.g. `list(mpg = list(mean = \(x) round(x, digits = 2) |> as.character))`.
#'
#' @param stat_labels ([`formula-list-selector`][syntax])\cr
#'   a named list, a list of formulas, or a single formula where
#'   the list element is either a named list or a list of formulas defining the
#'   statistic labels, e.g. `everything() ~ list(mean = "Mean", sd = "SD")` or
#'   `everything() ~ list(mean ~ "Mean", sd ~ "SD")`.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' ard_continuous(ADSL, by = "ARM", variables = "AGE")
#'
#' # equivalent to above
#' ADSL |>
#'   dplyr::group_by(ARM) |>
#'   ard_continuous(variables = "AGE")
ard_continuous <- function(data,
                           variables,
                           by = dplyr::group_vars(data),
                           strata = NULL,
                           statistics = everything() ~ continuous_variable_summary_fns(),
                           fmt_fn = NULL,
                           stat_labels = everything() ~ default_stat_labels()) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_class_data_frame(x = data)
  check_class(x = statistics, class = c("list", "formula"), allow_null = TRUE)
  check_class(x = stat_labels, class = c("list", "formula"), allow_null = TRUE)
  check_class(x = fmt_fn, class = c("list", "formula"), allow_null = TRUE)
  .check_no_ard_columns(data)

  # process arguments ----------------------------------------------------------
  # notify user if default `by` results in grouped results
  if (identical(
    call_match(defaults = TRUE)$by,
    formals(cards::ard_continuous)[["by"]]
  ) &&
    dplyr::is_grouped_df(data)) {
    cli::cli_inform("Results will be grouped by {.val {by}}")
  }
  data <- dplyr::ungroup(data)
  process_selectors(data, variables = {{ variables }}, by = {{ by }}, strata = {{ strata }})

  process_formula_selectors(
    data = data[variables],
    statistics = statistics,
    fmt_fn = fmt_fn,
    stat_labels = stat_labels
  )
  fill_formula_selectors(
    data = data[variables],
    statistics = formals(cards::ard_continuous)[["statistics"]] |> eval(),
    stat_labels = formals(cards::ard_continuous)[["stat_labels"]] |> eval()
  )

  check_list_elements(
    statistics = function(x) is.list(x) && is_named(x) && every(x, is.function),
    error_msg =
      list(
        statistics =
          c("Error in the argument {.arg {arg_name}} for variable {.val {variable}}.",
            "i" = "Value must be a named list of functions."
          )
      )
  )

  # return empty tibble if no variables selected -------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }

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
      new_col_name = "...ard_all_stats...",
      by = by,
      strata = strata,
      data = data
    )

  # unnest results
  df_results <-
    df_nested |>
    dplyr::select(all_ard_groups(), "...ard_all_stats...") |>
    tidyr::unnest(cols = "...ard_all_stats...")

  # add default function labels and formatters
  df_results <-
    df_nested |>
    dplyr::select(all_ard_groups(), "...ard_all_stats...") |>
    tidyr::unnest(cols = "...ard_all_stats...")

  # final processing of fmt_fn -------------------------------------------------
  df_results <-
    .process_nested_list_as_df(
      x = df_results,
      arg = fmt_fn,
      new_column = "statistic_fmt_fn"
    ) |>
    .default_fmt_fn()

  # final processing of stat labels --------------------------------------------
  df_results <-
    .process_nested_list_as_df(
      x = df_results,
      arg = stat_labels,
      new_column = "stat_label",
      unlist = TRUE
    ) |>
    dplyr::mutate(stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name))

  # add meta data and class ----------------------------------------------------
  df_results |>
    dplyr::mutate(context = "continuous") |>
    tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))} # styler: off
}



#' Check Protected Column Names
#'
#' Checks that column names in a passed data frame are not protected, that is,
#' they do not begin with `"...ard_"` and end with `"..."`
#'
#' @param x data frame
#' @param env environment for error messaging
#' @param exceptions character string of column names to exclude from checks
#'
#' @return invisible
#' @keywords internal
.check_no_ard_columns <- function(x, call = parent.frame(), exceptions = "...ard_dummy_for_counting...") {
  colnames <- names(x)
  ard_colnames <-
    colnames[startsWith(colnames, "...ard_") & endsWith(colnames, "...")] |>
    setdiff(exceptions)

  if (!is_empty(ard_colnames)) {
    "Columns beginning with {.val '...ard_'} and ending with {.val '...'} are not allowed." |>
      cli::cli_abort(call = call)
  }
}

#' Calculate Continuous Statistics
#'
#' Calculate statistics and return in an ARD format
#'
#' @param df_nested a nested data frame
#' @param variables character vector of variables
#' @param statistics named list of statistical functions
#'
#' @keywords internal
#' @return data frame
.calculate_stats_as_ard <- function(df_nested, variables, statistics,
                                    by, strata, data,
                                    new_col_name = "...ard_all_stats...") {
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
                      getOption(
                        "cards.calculate_stats_as_ard.eval_fun",
                        default = expr(do.call(fun, args = list(stats::na.omit(nested_data[[variable]]))))
                      )
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


#' Prepare Results as Data Frame
#'
#' Function take the results from `eval_capture_conditions()`, which is a
#' named list, e.g. `list(result=, warning=, error=)`, and converts it to a data
#' frame.
#'
#' @param x named list, the result from `eval_capture_conditions()`
#' @param variable string, variable name of the results
#' @param fun_name string, name of function called to get results in `x`
#'
#' @return data frame
#' @keywords internal
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


#' Convert Nested Lists to Column
#'
#' Some arguments, such as the `stat_label`, are passed as nested lists. This
#' function properly unnests these lists and adds the to the results data frame.
#'
#' @param x result data frame
#' @param arg the nested list
#' @param new_column string, new column name
#' @param unlist logical, whether to fully unlist final results
#'
#' @return a data frame
#' @keywords internal
.process_nested_list_as_df <- function(x, arg, new_column, unlist = FALSE) {
  # add statistic_fmt_fn column if not already present
  if (!new_column %in% names(x)) {
    x[[new_column]] <- list(NULL)
  }

  # process argument if not NULL, and update new column
  if (!is_empty(arg)) {
    df_argument <-
      imap(
        arg,
        function(enlst_arg, variable) {
          lst_stat_names <-
            x[c("variable", "stat_name")] |>
            dplyr::filter(.data$variable %in% .env$variable) |>
            unique() %>%
            {stats::setNames(as.list(.[["stat_name"]]), .[["stat_name"]])} # styler: off

          compute_formula_selector(
            data = lst_stat_names,
            x = enlst_arg
          ) %>%
            # styler: off
            {dplyr::tibble(
              variable = variable,
              stat_name = names(.),
              "{new_column}" := unname(.)
            )}
          # styler: on
        }
      ) |>
      dplyr::bind_rows()

    x <- x |> dplyr::rows_update(df_argument, by = c("variable", "stat_name"), unmatched = "ignore")
  }

  if (isTRUE(unlist)) {
    x[[new_column]] <- lapply(x[[new_column]], function(x) x %||% NA) |> unlist()
  }

  x
}

#' Add Default Formatting Functions
#'
#' @param data frame with cards structure
#'
#' @keywords internal
#' @return data frame
.default_fmt_fn <- function(x) {
  x |>
    dplyr::mutate(
      statistic_fmt_fn =
        map2(
          .data$stat_name, .data$statistic_fmt_fn,
          function(stat_name, statistic_fmt_fn) {
            if (!is_empty(statistic_fmt_fn)) {
              return(statistic_fmt_fn)
            }
            if (stat_name %in% c("n", "N", "N_obs", "N_miss", "N_nonmiss")) {
              return(0L)
            }
            if (stat_name %in% c("p", "p_miss", "p_nonmiss")) {
              return(label_cards(digits = 1, scale = 100))
            }

            return(1L)
          }
        )
    )
}
