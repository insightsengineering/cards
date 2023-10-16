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

  df_statsistics <-
    lapply(
      X = variables,
      FUN = function(x) {
        dplyr::tibble(
          variable = x,
          stat_name = names(statistics[[x]])
        )
      }
    ) |>
    dplyr::bind_rows() |>
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

  # calculate statistics -------------------------------------------------------
  df_return <-
    data |>
    .ard_nest(
      by = by,
      key = "...ard_nested_data..."
    )

  # add columns for the by variables
  if (!rlang::is_empty(by)) {
    df_return <-
      df_return |>
      # setting column names for stratum levels
      dplyr::mutate(!!!(as.list(by) |> stats::setNames(paste0("group", seq_along(by)))), .before = 0L) |>
      dplyr::rename(!!!(as.list(by) |> stats::setNames(paste0("group", seq_along(by), "_level"))))
  }

  df_return$..ard_all_stats.. <-
    lapply(
      df_return[["...ard_nested_data..."]],
      FUN = function(nested_data) {
        df_statsistics |>
          dplyr::mutate(
            result =
               map2(
                df_statsistics$variable,
                df_statsistics$stat_name,
                function(variable, stat_name) {
                  eval_capture_conditions(
                    do.call(statistics[[variable]][[stat_name]], args = list(nested_data[[variable]]))
                  ) |>
                    lapply(FUN = list) |>
                    dplyr::as_tibble() |>
                    dplyr::rename(statistic = "result")
                }
              )
          ) |>
          tidyr::unnest(cols = "result")
      }
    )

  df_return |>
    dplyr::select(-"...ard_nested_data...") |>
    tidyr::unnest(cols = "..ard_all_stats..") |>
    dplyr::mutate(context = "continuous") %>%
    structure(., class = c("card", class(.)))
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
