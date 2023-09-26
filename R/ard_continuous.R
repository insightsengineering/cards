#' Continuous ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for simple continuous summary statistics.
#'
#' @param data a data frame
#' @param by columns to compute statistics by. Default are the columns
#' returned by `dplyr::group_vars(data)`.
#' @param statistics a named list of functions that return a summary statistic,
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
                           statistics = NULL) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data, "data")
  check_class_data_frame(data = data)
  check_class(class = "list", statistics = statistics, allow_null = TRUE)

  # process arguments ----------------------------------------------------------
  .process_selecting_args(data, variables = {{variables}}, by = {{by}})

  # setting default statistics -------------------------------------------------
  statistics <-
    variables |>
    lapply(function(x) statistics[[x]] %||% .default_continuous_statistics()) |>
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

.default_continuous_statistics <- function() {
  list(
    N = function(x) sum(!is.na(x)),
    # N_miss = function(x) sum(is.na(x)),
    length = function(x) length(x),
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
