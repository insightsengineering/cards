#' ARD Attributes
#'
#' @description
#' Add variable attributes to an ARD data frame.
#' - The `label` attribute will be added for all columns, and when no label
#'   is specified and no label has been set for a column using the `label=` argument,
#'   the column name will be placed in the label statistic.
#' - The `class` attribute will also be returned for all columns.
#' - Any other attribute returned by `attributes()` will also be added, e.g. factor levels.
#'
#' @rdname ard_attributes
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables to include
#' @param label (named `list`)\cr
#'   named list of variable labels, e.g. `list(cyl = "No. Cylinders")`.
#'   Default is `NULL`
#' @inheritParams rlang::args_dots_empty
#'
#' @return an ARD data frame of class 'card'
#' @name ard_attributes
#'
#' @examples
#' df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
#' attr(df$var1, "label") <- "Lowercase Letters"
#'
#' ard_attributes(df, variables = everything())
NULL

#' @rdname ard_attributes
#' @export
ard_attributes <- function(data, ...) {
  UseMethod("ard_attributes")
}

#' @rdname ard_attributes
#' @export
ard_attributes.data.frame <- function(data,
                                      variables = everything(),
                                      label = NULL,
                                      ...) {
  set_cli_abort_call()
  check_dots_used()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_dots_empty()

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  process_selectors(data, variables = {{ variables }})

  # return empty ARD if no variables selected ----------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble() |> as_card())
  }


  # check label is a named list ------------------------------------------------
  if (!is_empty(label)) {
    if (!is.list(label) || !is_named(label) || some(label, \(x) !is_string(x))) {
      cli::cli_abort(
        "The {.arg label} argument must be a named list with each element a string.",
        call = get_cli_abort_call()
      )
    }
  }



  variables |>
    lapply(
      FUN = function(y) {
        attr <- attributes(data[[y]])
        # add/update variable label
        attr[["label"]] <- label[[y]] %||% attr[["label"]] %||% y
        # attributes() doesn't always return class, adding it if not already present
        attr[["class"]] <- attr[["class"]] %||% class(data[[y]])

        dplyr::tibble(
          variable = .env$y,
          stat_name = names(attr),
          stat = unname(attr)
        )
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      stat_label = dplyr::case_when(
        .data$stat_name %in% "label" ~ "Variable Label",
        .data$stat_name %in% "class" ~ "Variable Class",
        TRUE ~ .data$stat_name
      ),
      context = "attributes",
      fmt_fn = ifelse(.data$stat_name %in% "label", list(as.character), list(NULL)),
      warning = list(NULL),
      error = list(NULL)
    ) |>
    cards::tidy_ard_column_order() |>
    as_card()
}

#' @rdname ard_attributes
#' @export
ard_attributes.default <- function(data, ...) {
  set_cli_abort_call()
  cli::cli_abort("There is no method for objects of class {.cls {class(data)}}.", call = get_cli_abort_call())
}
