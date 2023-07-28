#' ARD Labels
#'
#' Add variable labels to an ARD data frame.
#' When no label specified and no label has been set for a column,
#' the column name will be placed in the label statistic.
#'
#'
#' @param data a data frame
#' @param label named list of variable labels, e.g. `list(mpg = "MPG")`.
#' Default is `NULL`
#' @param include variable to include
#'
#' @return a data frame
#' @export
#'
#' @examples
#' df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
#' attr(df$var1, 'label') <- "Lowercase Letters"
#'
#' ard_label(df)
ard_label <- function(data, label = NULL, include = everything()) {
  include <- dplyr::select(data, {{ include }}) |> colnames()

  dplyr::tibble(variable = include) |>
    dplyr::mutate(
      stat_name = "label",
      statistic =
        lapply(
          .data$variable,
          function(x) label[[x]] %||% attr(x, "label") %||% x
        )
    )
}
