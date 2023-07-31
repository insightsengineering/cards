#' Convert Table to Print Engine Class
#'
#' Convert a 'cardinal' table to any of the print engine classes.
#' Goal is to support gt, rtables, flextable, huxtable, kableExtra,
#' kable, and formatted tibbles
#'
#' @param x a cardinal table
#' @param engine a string indicating the print engine. Default is `"gtß"`
#'
#' @return a table object
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
#' construct_cardinal(
#'   plan_table =
#'     dplyr::bind_rows(
#'       table_ard |> dplyr::filter(variable %in% "mpg") |>  plan_table_simple_continuous(),
#'       table_ard |> dplyr::filter(variable %in% "am") |> plan_table_simple_categorical()
#'     ),
#'   plan_header =
#'     table_ard |>
#'     dplyr::filter(variable %in% "cyl") |>
#'     plan_header_simple(header = "**{strata} Cylinders  \nN = {n}  ({p}%)**") |>
#'     modifyList(val = list(label = gt::md("**Characteristic**")))
#' ) |>
#'   convert_cardinal()
convert_cardinal <- function(x, engine = "gt") {
  # check inputs ---------------------------------------------------------------
  engine <- match.arg(engine)
  if (!inherits(x, "cardinal")) {
    cli::cli_abort("Argument {.code x} must be class {.cls cardinal}")
  }

  # construct table (assuming it's a gt table for now) -------------------------
  # this gt-specific code will be migrated to its own function
  gt::gt(x$table_body) |>
    gt::sub_missing(missing_text = "") |>
    gt::cols_label(.list = x$table_styling$header) |>
    gt::cols_hide(columns = all_of(x$table_styling$hide))
}
