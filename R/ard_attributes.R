#' ARD Attributes
#'
#' Add variable attributes to an ARD data frame.
#' - The `label` attribute will be added for all columns, and when no label
#'   is specified and no label has been set for a column using the `label=` argument,
#'   the column name will be placed in the label statistic.
#' - The `class` attribute will also be returned for all columns.
#' - Any other attribute returned by `attributes()` will also be added, e.g. factor levels.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param label (named `list`)\cr
#'   named list of variable labels, e.g. `list(cyl = "No. Cylinders")`.
#'   Default is `NULL`
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables to include
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
#' attr(df$var1, "label") <- "Lowercase Letters"
#'
#' ard_attributes(df, variables = everything())
ard_attributes <- function(data, variables = everything(), label = NULL) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_data_frame(x = data)

  # process arguments ----------------------------------------------------------
  data <- dplyr::ungroup(data)
  process_selectors(data, variables = {{ variables }})

  variables |>
    lapply(
      FUN = function(x) {
        attr <- attributes(data[[x]])
        # add/update variable label
        attr[["label"]] <- label[[x]] %||% attr[["label"]] %||% x
        # attributes() doesn't always return class, adding it if not already present
        attr[["class"]] <- attr[["class"]] %||% class(data[[x]])

        dplyr::tibble(
          variable = .env$x,
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
