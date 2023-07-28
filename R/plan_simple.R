#' Simple ARD Plans
#'
#' Plans to convert simple ARDs to formatted data frames.
#' These data frames will eventually be stacked into a final table.
#'
#'
#' @param ard a single ARD object for one variable
#' @param statistics a character vector of statistics to include in
#' resulting table. Default is `c("{N}", "{mean} ({sd})",  "{min}, {max}")`
#'
#' @return data frame
#' @name plan_simple
#'
#' @examples
#' ard_continuous(mtcars, by = cyl, include = mpg) |>
#'   plan_simple_continuous()
NULL

#' @rdname plan_simple
#' @export
plan_simple_continuous <- function(ard, statistics = c("{N}", "{mean} ({sd})",  "{min}, {max}")) {
  # check the function inputs match availability in the ARD --------------------

  # construct table body -------------------------------------------------------
  # the nested ARD object is one line per level of the by variable
  nested_ard <-
    ard |>
    tidyr::drop_na() |> # this drops the label stat row that doesn't have a strata value
    dplyr::select(-"context") |>
    dplyr::mutate(
      statistic =
        lapply(
          .data$statistic,
          function(x) ifelse(is.numeric(x), round(x), x %||% NA_character_)
        ) |>
        unlist() |>
        as.character()
    ) |>
    tidyr::nest(
      .by = c("strata1", "strata1_level", "variable")
    ) |>
    dplyr::mutate(
      col_name = paste0("stat_", dplyr::row_number())
    )

  # the table_body object is essentially the df that will be printed
  table_body <-
    nested_ard |>
    dplyr::mutate(
      fmt_statistics =
        lapply(
          .data$data,
          FUN = function(x) {
            lapply(
              statistics,
              function(one_stat) {
                glue::glue(one_stat, .envir = rlang::as_environment(.data_frame_to_named_list(x)))
              }
            ) |>
              unlist()
          }
        ),
      stat_label = list(statistics)
    ) |>
    dplyr::select(-"data") |>
    tidyr::unnest(cols = c("fmt_statistics", "stat_label")) |>
    tidyr::pivot_wider(
      id_cols = c("variable", "stat_label"),
      names_from = "col_name",
      values_from = "fmt_statistics"
    ) |>
    dplyr::mutate(
      label =
        dplyr::if_else(
          dplyr::row_number() == 1L,
          .get_ard_label_statistic(ard) %||% .data$variable,
          NA_character_
        ),
      .after = "variable"
    )

  # return table_body data frame -----------------------------------------------
  table_body
}


#' @rdname plan_simple
#' @export
plan_simple_categorical <- function(ard, statistics = c("{n}", "({p})%")) {
  # check the function inputs match availability in the ARD --------------------

  # construct table body -------------------------------------------------------

}


