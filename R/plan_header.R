#' Column Header Plan
#'
#' Create a named list column header definitions for statistic columns
#'
#' @param ard a ARD object
#' @param header a string specifying the form of a column header.
#' Default is `"{group}  \nN = {n}"`
#'
#' @return named list
#' @export
#'
#' @examples
#' ard_categorical(mtcars, variables = "cyl") |>
#'   header_plan_simple(header = "**{group} Cylinders**  \nN = {n} ({p}%)")

# TODO: Update function to handle OVERALL-only tables
# TODO: the returned text string is formatted with `gt::md()`. Should this be optional?
header_plan_simple <- function(ard, header = "{group}  \nN = {n}") {
  nested_ard <-
    ard |>
    # TODO: DELETE THIS mutate() LATER. NEED BETTER SOLUTION TO INCLUDE VARIABLE-LEVEL SUMMARY STATS
    dplyr::select("variable", "stat_name", "statistic", "context", "variable_level") |>
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
      .by = c("variable", "variable_level")
    ) |>
    dplyr::mutate(
      col_name = paste0("stat_", dplyr::row_number())
    )

  nested_ard |>
    dplyr::mutate(
      data =
        .map2(
          .data$data, .data$variable_level,
          function(data, level) {
            dplyr::bind_rows(
              data,
              dplyr::tibble(stat_name = "group", statistic = level)
            )
          }
        ),
      fmt_statistics =
        lapply(
          .data$data,
          FUN = function(x) {
            lapply(
              header,
              function(one_stat) {
                glue::glue(one_stat, .envir = rlang::as_environment(.data_frame_to_named_list(x)))
              }
            ) |>
              unlist()
          }
        )
    )|>
    dplyr::select(-"data") %>%
    {.data_frame_to_named_list(.[c("col_name", "fmt_statistics")])} |>
    lapply(FUN = function(x) gt::md(x))
}
