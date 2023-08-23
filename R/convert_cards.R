#' Convert Table to Print Engine Class
#'
#' Convert a 'cards' table to any of the print engine classes.
#' Goal is to support gt, rtables, flextable, huxtable, kableExtra,
#' kable, and formatted tibbles
#'
#' @param x a cards table
#' @param engine a string indicating the print engine. Default is `"gtß"`
#'
#' @return a table object
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
#' construct_cards(
#'   table_plan =
#'     dplyr::bind_rows(
#'       table_ard |> dplyr::filter(variable %in% "mpg") |>  table_plan_simple_continuous(),
#'       table_ard |> dplyr::filter(variable %in% "am") |> table_plan_simple_categorical()
#'     ),
#'   header_plan =
#'     table_ard |>
#'     dplyr::filter(variable %in% "cyl") |>
#'     header_plan_simple(header = "**{group} Cylinders  \nN = {n}  ({p}%)**") |>
#'     modifyList(val = list(label = gt::md("**Characteristic**")))
#' ) |>
#'   convert_cards()
convert_cards <- function(x, engine = "gt") {
  # check inputs ---------------------------------------------------------------
  engine <- match.arg(engine)
  if (!inherits(x, "cards")) {
    cli::cli_abort("Argument {.code x} must be class {.cls cards}")
  }

  # construct table (assuming it's a gt table for now) -------------------------
  # this gt-specific code will be migrated to its own function
  gt::gt(x$table_body) |>
    gt::sub_missing(missing_text = "") |>
    gt::cols_label(.list = x$table_styling$header) |>
    gt::cols_hide(columns = all_of(x$table_styling$hide)) |>
    # TODO: the styling below should belong in the cards object
    # add indentation
    gt::text_transform(
      locations = gt::cells_body(
        columns = "label",
        rows = !.data$header_row
      ),
      fn = function(x) paste0("\U00A0\U00A0\U00A0\U00A0", x)
    ) |>
    # bold variable header rows
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        columns = "label",
        rows = .data$header_row
      )
    )
}
