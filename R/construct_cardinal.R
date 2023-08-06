#' Construct Cardinal Table
#'
#' Construct a table of class 'cardinal' from the table plan, header plan, and
#' other table compnents
#'
#' @param table_plan a table plan data frame
#' @param header_plan a header plan named list
#' @param hide character vector of columns to hide from output
#'
#' @return a cardinal table
#' @export
#'
#' @examples
#' # Construct the ARD
#' table_ard <-
#'   dplyr::bind_rows(
#'     ard_continuous(mtcars, by = cyl, include = mpg),
#'     ard_categorical(mtcars, by = cyl, include = am),
#'     ard_categorical(mtcars,  include = cyl)
#'   )
#'
#' # convert ARD to a cardinal table
#' table <-
#'   construct_cardinal(
#'     table_plan =
#'       dplyr::bind_rows(
#'         table_ard |> dplyr::filter(variable %in% "mpg") |>  table_plan_simple_continuous(),
#'         table_ard |> dplyr::filter(variable %in% "am") |> table_plan_simple_categorical()
#'       ),
#'     header_plan =
#'       table_ard |>
#'       dplyr::filter(variable %in% "cyl") |>
#'       header_plan_simple(header = "**{strata} Cylinders  \nN = {n} ({p}%)**") |>
#'       utils::modifyList(val = list(label = "**Characteristic**"))
#'   )
construct_cardinal <- function(table_plan,
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
    structure(class = "cardinal")
}
