#' Simple ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for simple summary statistics from
#' continuous and categorical data.
#'
#' @param data a data frame
#' @param by columns to compute statistics by. Default are the columns
#' returned by `dplyr::group_vars(data)`.
#' @param statistics a named list of functions that return a summary statistic,
#' e.g. `list(mpg = list(mean = \(x) mean(x, na.rm = TRUE)))`
#' @param variables columns to include in summaries. Default is `everything()`.
#'
#' @return a data frame
#' @name ard_simple
#'
#' @examples
#' ard_continuous(mtcars, by = "cyl", variables = c("mpg", "hp"))
#' ard_categorical(mtcars, by = "cyl", variables = c("am", "gear"))
NULL

#' @rdname ard_simple
#' @export
ard_continuous <- function(data,
                           by = dplyr::group_vars(data),
                           variables = everything(),
                           statistics = NULL) {
  # process arguments -----------------------------------------------------------
  by <- dplyr::select(data, {{ by }}) |> colnames()
  all_summary_variables <- dplyr::select(data, {{ variables }}) |> colnames() |> setdiff(by)
  data <- dplyr::ungroup(data)

  # check inputs (will make this more robust later) ----------------------------

  # setting default statistics -------------------------------------------------
  statistics <-
    all_summary_variables |>
    lapply(function(x) statistics[[x]] %||% .default_continuous_statistics()) |>
    stats::setNames(nm = all_summary_variables)

  df_statsistics <-
    lapply(
      X = all_summary_variables,
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
      by = all_of(by),
      key = "...ard_nested_data..."
    )

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
              .map2(
                df_statsistics$variable,
                df_statsistics$stat_name,
                function(variable, stat_name) {
                  eval_capture_conditions(
                    do.call(statistics[[variable]][[stat_name]], args = list(nested_data[[variable]]))
                  ) |>
                    lapply(FUN = list) |>
                    dplyr::as_tibble() |>
                    dplyr::rename(statistic = .data$result)
                }
              )
          ) |>
          tidyr::unnest(cols = "result")
      }
    )

  df_return |>
    dplyr::select(-"...ard_nested_data...") |>
    tidyr::unnest(cols = "..ard_all_stats..") |>
    dplyr::mutate(context = list("continuous")) %>%
    structure(., class = c("card", class(.)))
}

#' @rdname ard_simple
#' @export
ard_categorical <- function(data, by = dplyr::group_vars(data), variables = everything()) {
  # process arguments -----------------------------------------------------------
  by <- dplyr::select(data, {{ by }}) |> colnames()
  all_summary_variables <- dplyr::select(data, {{ variables }}) |> colnames() |> setdiff(by)
  data <- dplyr::ungroup(data)

  # check inputs (will make this more robust later) ----------------------------

  # calculating summary stats --------------------------------------------------
  # first, calculating variable-level stats
  statistics <-
    rep_len(
      list(.default_continuous_statistics()[c("N", "N_miss", "N_tot")]),
      length.out = length(all_summary_variables)
    ) |>
    stats::setNames(nm = all_summary_variables)

  df_ard <-
    ard_continuous(
      data = data,
      by = !!all_of(by),
      statistics = statistics,
      variables = !!all_of(all_summary_variables)
    )

  # second, tabulate variable
  df_ard_tablulation <-
    lapply(
      X = all_summary_variables,
      FUN = function(v) {
        ard_continuous(
          data = data |> dplyr::select(all_of(c(by, v))) |> tidyr::drop_na(),
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
    dplyr::bind_rows() |>
    dplyr::rows_update(
      .default_statistic_labels(),
      by = "stat_name",
      unmatched = "ignore"
    )

  # bind data frames with stats, and return to user ----------------------------
  dplyr::bind_rows(df_ard_tablulation, df_ard) |>
    dplyr::mutate(context = list("categorical")) %>%
    structure(., class = c("card", class(.)))
}





.default_continuous_statistics <- function() {
  list(
    N = function(x) sum(!is.na(x)),
    N_miss = function(x) sum(is.na(x)),
    N_tot = function(x) length(x),
    mean = function(x) mean(x, na.rm = TRUE),
    sd = function(x) stats::sd(x, na.rm = TRUE),
    min = function(x) min(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE)
  )
}

.default_statistic_labels <- function() {
  list(
    mean = "Mean",
    sd = "SD",
    var = "Variance",
    min = "Min",
    max = "Max",
    n = "n",
    N_miss = "N Missing",
    N_tot = "Total N",
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
    N_tot = function(x) format(round(x, digits = 0), nsmall = 0),
    p = function(x) format(round(x * 100, digits = 1), nsmall = 1),
    p_cell = function(x) format(round(x * 100, digits = 1), nsmall = 1)
  ) %>%
    {dplyr::tibble(
      stat_name = names(.),
      statistic_fmt_fn = unname(.)
    )}
}
