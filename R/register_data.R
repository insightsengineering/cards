#' Register Data Frame
#'
#' - `register_data()`: Register a data frame
#' - `add_registered_data_column()`: Add the data registration information to the ARD
#'
#' @param data a data frame, e.g. `ADSL`.
#' @param filter_expr an expression that will be applied to the data frame passed in `data`.
#' @param data_name string of the data frame name. Default is `rlang::caller_arg(data)`.
#' @param filter_label string describing the filter. The default is the string version
#'   filter expression.
#'
#' @returns data frame
#' @name register_data
#'
#' @examples
NULL

#' @export
#' @rdname register_data
register_data <- function(data,
                          filter_expr = TRUE,
                          data_name = rlang::caller_arg(data),
                          filter_label = rlang::expr_deparse(filter_expr, width = Inf)) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_data_frame(data)
  if (!missing(filter_expr)) {
    filter_expr <- rlang::enexpr(filter_expr)
    if (!rlang::is_call_simple(filter_expr)) {
      cli::cli_abort(
        c("The {.arg filter_expr} argument must be a simple {.emph simple} call.",
          "i" = "See {.fun is_call_simple} for details."),
        call = get_cli_abort_call()
      )
    }
  }
  check_string(data_name)
  check_string(filter_label)

  # Add column and apply filter --------------------------------------------------
  data <-
    data |>
    dplyr::filter(!!filter_expr) |>
    structure(class = c("cards_data", class(data)))

  attr(data, "cards_data_desc") <-
    list(data_name = data_name, filter_label = filter_label) |>
    structure(class = "cards_data_desc")

  data
}

#' @export
#' @rdname register_data
add_registered_data_column <- function(x, data) {
  # if not a registered data frame, return the ARD as it is
  if (!inherits(data, "cards_data")) return(x)

  x |>
    dplyr::mutate(
      .before = 1L,
      data = list(attr(data, "cards_data_desc"))
    )
}

#' @export
#' @rdname register_data
print.cards_data_desc <- function(x) {
  paste(data_name, filter_label, sep = ": ") |>
    print()
}
