#' Construct cards Table
#'
#' Construct a table of class 'cards' from the table plan, header plan, and
#' other table components
#'
#' @param table_plan a table plan data frame
#' @param header_plan a header plan named list
#' @param hide character vector of columns to hide from output
#'
#' @return a cards table
#' @export
#' @keywords internal
#'
#' @examples
#' # Construct the ARD
#' table_ard <-
#'   dplyr::bind_rows(
#'     ard_continuous(mtcars, by = cyl, variables = "mpg"),
#'     ard_categorical(mtcars, by = cyl, variables = "am"),
#'     ard_categorical(mtcars,  variables = "cyl")
#'   )
#'
#' # convert ARD to a cards table
#' table <-
#'   construct_cards(
#'     table_plan =
#'       dplyr::bind_rows(
#'         table_ard |> dplyr::filter(variable %in% "mpg") |>  table_plan_simple_continuous(),
#'         table_ard |> dplyr::filter(variable %in% "am") |> table_plan_simple_categorical()
#'       ),
#'     header_plan =
#'       table_ard |>
#'       dplyr::filter(variable %in% "cyl") |>
#'       header_plan_simple(header = "**{group} Cylinders  \nN = {n} ({p}%)**") |>
#'       utils::modifyList(val = list(label = "**Characteristic**"))
#'   )
construct_cards <- function(table_plan,
                               header_plan,
                               hide = c("variable", "header_row")) {
  list(
    table_body = table_plan,
    table_styling =
      list(
        header = header_plan,
        hide = hide
      )
  ) |>
    structure(class = "cards")
}
