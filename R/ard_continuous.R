#' Continuous ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for simple continuous summary statistics.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to include in summaries. Default is `everything()`.
#' @param by,strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to tabulate by/stratify by for summary statistic
#'   calculation. Arguments are similar, but with an important distinction:
#'
#'   `by`: results are calculated for **all combinations** of the columns specified,
#'      including unobserved combinations and unobserved factor levels.
#'
#'   `strata`: results are calculated for **all _observed_ combinations** of the
#'     columns specified.
#'
#'   Arguments may be used in conjunction with one another.
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
#' @param fmt_fn ([`formula-list-selector`][syntax])\cr
#'   a named list, a list of formulas,
#'   or a single formula where the list element is a named list of functions
#'   (or the RHS of a formula),
#'   e.g. `list(mpg = list(mean = \(x) round(x, digits = 2) |> as.character))`.
#' @param stat_labels ([`formula-list-selector`][syntax])\cr
#'   a named list, a list of formulas, or a single formula where
#'   the list element is either a named list or a list of formulas defining the
#'   statistic labels, e.g. `everything() ~ list(mean = "Mean", sd = "SD")` or
#'   `everything() ~ list(mean ~ "Mean", sd ~ "SD")`.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_continuous(ADSL, by = "ARM", variables = "AGE")
#'
#' # if a single function returns a named list, the named
#' # results will be placed in the resulting ARD
#' ADSL |>
#'   dplyr::group_by(ARM) |>
#'   ard_continuous(
#'     variables = "AGE",
#'     statistics =
#'       ~ list(conf.int = \(x) t.test(x)[["conf.int"]] |>
#'         as.list() |>
#'         setNames(c("conf.low", "conf.high")))
#'   )
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
  check_data_frame(x = data)
  check_class(x = statistics, cls = c("list", "formula"), allow_empty = TRUE)
  check_class(x = stat_labels, cls = c("list", "formula"), allow_empty = TRUE)
  check_class(x = fmt_fn, cls = c("list", "formula"), allow_empty = TRUE)
  .check_no_ard_columns(data)

  # process arguments ----------------------------------------------------------
  process_selectors(data,
    variables = {{ variables }},
    by = {{ by }},
    strata = {{ strata }}
  )
  data <- dplyr::ungroup(data)

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
    x = statistics,
    predicate = function(x) is.list(x) && is_named(x) && every(x, is.function),
    error_msg =
      c("Error in the argument {.arg {arg_name}} for variable {.val {variable}}.",
        "i" = "Value must be a named list of functions."
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
#' they do not begin with `"...ard_"` and end with `"..."`.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#' @param call (`environment`)\cr
#'   frame for error messaging. Default is [parent.frame()].
#' @param exceptions (`string`)\cr
#'   character string of column names to exclude from checks
#'
#' @return returns invisible if check is successful, throws an error message if not.
#' @keywords internal
#'
#' @examples
#' data <- data.frame("ard_x" = 1)
#'
#' cards:::.check_no_ard_columns(data)
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
#' @param df_nested (`data.frame`)\cr
#'   a nested data frame
#' @param variables (`character`)\cr
#'   character vector of variables
#' @param statistics (named `list`)\cr
#'   named list of statistical functions
#'
#' @return an ARD data frame of class 'card'
#' @keywords internal
#'
#' @examples
#' data_nested <- ADSL |>
#'   nest_for_ard(
#'     by = "ARM",
#'     strata = NULL,
#'     key = "...ard_nested_data..."
#'   )
#'
#' cards:::.calculate_stats_as_ard(
#'   df_nested = data_nested,
#'   variables = "AGE",
#'   statistics = list(mean = "mean"),
#'   by = "ARM",
#'   strata = NULL,
#'   data = ADSL
#' )
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
#' Function takes the results from [eval_capture_conditions()], which is a
#' named list, e.g. `list(result=, warning=, error=)`, and converts it to a data
#' frame.
#'
#' @param x (named `list`)\cr
#'   the result from [eval_capture_conditions()]
#' @param variable (`string`)\cr
#'   variable name of the results
#' @param fun_name (`string`)\cr
#'   name of function called to get results in `x`
#'
#' @return a data frame
#' @keywords internal
#'
#' @examples
#' msgs <- eval_capture_conditions({
#'   warning("Warning 1")
#'   warning("Warning 2")
#'   letters[1:2]
#' })
#'
#' cards:::.lst_results_as_df(msgs, "result", "mean")
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
#' Some arguments, such as `stat_label`, are passed as nested lists. This
#' function properly unnests these lists and adds them to the results data frame.
#'
#' @param x (`data.frame`)\cr
#'   result data frame
#' @param arg (`list`)\cr
#'   the nested list
#' @param new_column (`string`)\cr
#'   new column name
#' @param unlist (`logical`)\cr
#'   whether to fully unlist final results
#'
#' @return a data frame
#' @keywords internal
#'
#' @examples
#' ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
#'
#' cards:::.process_nested_list_as_df(ard, NULL, "new_col")
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
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#'
#' @return a data frame
#' @keywords internal
#'
#' @examples
#' ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1") |>
#'   dplyr::mutate(statistic_fmt_fn = NA)
#'
#' cards:::.default_fmt_fn(ard)
.default_fmt_fn <- function(x) {
  x |>
    dplyr::mutate(
      statistic_fmt_fn =
        pmap(
          list(.data$stat_name, .data$statistic, .data$statistic_fmt_fn),
          function(stat_name, statistic, statistic_fmt_fn) {
            if (!is_empty(statistic_fmt_fn)) {
              return(statistic_fmt_fn)
            }
            if (stat_name %in% c("p", "p_miss", "p_nonmiss")) {
              return(label_cards(digits = 1, scale = 100))
            }
            if (is.integer(statistic)) {
              return(0L)
            }
            if (is.numeric(statistic)) {
              return(1L)
            }
            return(as.character)
          }
        )
    )
}