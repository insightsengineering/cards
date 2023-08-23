#' Simple ARD Table Plans
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
#' ard_continuous(mtcars, by = cyl, variables = "mpg") |>
#'   table_plan_simple_continuous()
#'
#' ard_categorical(mtcars, by = cyl, variables = "am") |>
#'   table_plan_simple_categorical()
NULL

# TODO: The continuous plan broke at some point. But not fixing now because this pkg doesn't that pkg

# TODO: These plans assume there is a stratifying variable, which is not always the case
# TODO: The plans only allow the calculation of one variable at a time. Would it be computationally faster to allow more than one, but then the results would need to be sorted on the final table construction side.
# TODO: These plans assume a single stratifying variable, when it would be great to support more.

#' @rdname plan_simple
#' @export
#' @keywords internal
table_plan_simple_continuous <- function(ard, statistics = c("{N}", "{mean} ({sd})",  "{min}, {max}")) {
  # check the function inputs match availability in the ARD --------------------

  # construct table body -------------------------------------------------------
  # the nested ARD object is one line per level of the by variable
  nested_ard <-
    ard |>
    dplyr::select(-any_of("variable_level")) |>
    tidyr::drop_na() |> # this drops the label stat row that doesn't have a group value
    dplyr::select(-any_of("context")) |>
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
      .by = c("group1", "group1_level", "variable")
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
      label = list(statistics),
      header_row = FALSE
    ) |>
    dplyr::select(-"data") |>
    tidyr::unnest(cols = c("fmt_statistics", "label", "header_row")) |>
    tidyr::pivot_wider(
      id_cols = c("variable", "label", "header_row"),
      names_from = "col_name",
      values_from = "fmt_statistics"
    ) %>%
    dplyr::bind_rows(
      dplyr::tibble(
        header_row = TRUE,
        label = .get_ard_label_statistic(ard)
      ),
      .
    ) |>
    tidyr::fill("variable", .direction = "up") |>
    dplyr::relocate("variable", .before = 0L)

  # return table_body data frame -----------------------------------------------
  table_body
}


#' @rdname plan_simple
#' @export
#' @keywords internal
table_plan_simple_categorical <- function(ard, statistics = "{n} ({p}%)") {
  # check the function inputs match availability in the ARD --------------------
  # construct table body -------------------------------------------------------
  # the nested ARD object is one line per level of the by variable
  nested_ard <-
    ard |>
    dplyr::select(-any_of(c("error", "warning"))) |>
    # TODO: DELETE THIS mutate() LATER. NEED BETTER SOLUTION TO INCLUDE VARIABLE-LEVEL SUMMARY STATS
    dplyr::mutate(dplyr::across("variable_level", ~lapply(., \(x) if (!is.null(x)) x else NA) |> unlist() |> as.character())) |>
    tidyr::drop_na() |> # this drops the label stat row that doesn't have a group value
    dplyr::select(-any_of("context")) |>
    dplyr::mutate(
      variable_level = unlist(.data$variable_level),
      statistic =
        lapply(
          .data$statistic,
          function(x) {
            # TODO: This needs to be updated with proper formatting functions that can be changed.
            ifelse(rlang::is_integerish(x), round(x), round(x * 100, digits = 1) %||% NA_character_)
          }
        ) |>
        unlist() |>
        as.character()
    ) |>
    tidyr::nest(
      .by = c("group1", "group1_level", "variable")
    ) |>
    dplyr::mutate(
      col_name = paste0("stat_", dplyr::row_number())
    )

  # the table_body object is essentially the df that will be printed
  table_body <-
    nested_ard |>
    # TODO: There must be a better way to do this beside an unnest() followed by a nest()
    tidyr::unnest(cols = "data") |>
    tidyr::nest(data = c("stat_name", "statistic")) |>
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
      label = .data$variable_level |> as.character(),
      header_row = FALSE
    ) |>
    dplyr::select(-"data") |>
    tidyr::unnest(cols = c("fmt_statistics", "label", "header_row")) |>
    tidyr::pivot_wider(
      id_cols = c("variable", "label", "header_row"),
      names_from = "col_name",
      values_from = "fmt_statistics"
    ) %>%
    dplyr::bind_rows(
      dplyr::tibble(
        header_row = TRUE,
        label = .get_ard_label_statistic(ard)
      ),
      .
    ) |>
    tidyr::fill("variable", .direction = "up") |>
    dplyr::relocate("variable", .before = 0L)

  table_body
}


