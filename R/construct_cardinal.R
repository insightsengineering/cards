#' Construct Cardinal Table
#'
#' Construct a table of class 'cardinal' from the table plan, header plan, and
#' other table compnents
#'
#' @param plan_table a table plan data frame
#' @param plan_header a header plan named list
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
#'     # TODO: The ARD creation code can by simplified after the categorical
#'     #       ARD accepts no-by variable specifications
#'     mtcars |>
#'       dplyr::mutate(..one.. = 1L) |>
#'       ard_categorical(by = ..one..,  include = cyl) |>
#'       dplyr::select(-starts_with("strata"))
#'   )
#'
#' # convert ARD to a cardinal table
#' table <-
#'   construct_cardinal(
#'     plan_table =
#'       dplyr::bind_rows(
#'         table_ard |> dplyr::filter(variable %in% "mpg") |>  plan_table_simple_continuous(),
#'         table_ard |> dplyr::filter(variable %in% "am") |> plan_table_simple_categorical()
#'       ),
#'     plan_header =
#'       table_ard |>
#'       dplyr::filter(variable %in% "cyl") |>
#'       plan_header_simple(header = "**{strata} Cylinders  \nN = {n} ({p}%)**") |>
#'       utils::modifyList(val = list(label = "**Characteristic**"))
#'   )
construct_cardinal <- function(plan_table,
                               plan_header,
                               hide = c("variable", "header_row")) {
  list(
    table_body = plan_table,
    table_styling =
      list(
        header = plan_header,
        hide = hide
      )
  ) |>
    structure(class = "cardinal")
}
