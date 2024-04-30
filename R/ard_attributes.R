#' Add variable attributes to an ARD object
#'
#' @description
#' ard_attributes S3 generic providing methods for the following classes:
#'
#' * `data.frame`: Adds variable attributes to an ARD data frame.
#'
#' Add variable attributes to an ARD data frame.
#' - The `label` attribute will be added for all columns, and when no label
#'   is specified and no label has been set for a column using the `label=` argument,
#'   the column name will be placed in the label statistic.
#' - The `class` attribute will also be returned for all columns.
#' - Any other attribute returned by `attributes()` will also be added, e.g. factor levels.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables to include
#' @param label (named `list`)\cr
#'   named list of variable labels, e.g. `list(cyl = "No. Cylinders")`.
#'   Default is `NULL`
#'
#' @return an ARD data frame of class 'card'
#'
#' @examples
#' df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
#' attr(df$var1, "label") <- "Lowercase Letters"
#'
#' ard_attributes(df, variables = everything())
#'
#' @export
#'
ard_attributes <- function(x, ...) {
  UseMethod("ard_attributes")
}


#' @rdname ard_attributes
#' @export
ard_attributes.data.frame <- function(x,
                                      variables = everything(),
                                      label = NULL) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(variables)
  check_data_frame(x)

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(x)
  process_selectors(data, variables = {{ variables }})

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
      context = "attributes"
    ) %>%
    {structure(., class = c("card", class(.)))} # styler: off
}

#' @rdname ard_attributes
#' @export
ard_attributes.default <- function(x) {
  stop("There is no method for object of class: ", paste(class(x), collapse = ", "))
}
