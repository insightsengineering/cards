#' ARD Helpers
#'
#' Helper functions for working with ARD data frames.
#'
#' - `get_ard_statistics()`: This function returns the statistics from an ARD
#'   as a named list. The statistic label, formatting instruction, and any error
#'   or warning messages are returned as attributes.
#'
#' - `bind_ard()`: A wrapper for `dplyr::bind_rows()` with additional checks
#'   for duplicated statistics
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param ...
#' - `get_ard_statistics()`: optional named arguments indicating rows to subset of the ARD.
#'   For example, to return only rows where the column `"AGEGR1"` is `"65-80"`,
#'   pass `AGEGR1 %in% "65-80"`.
#' - `bind_ard()`: ARDs to combine. Each argument can either be an ARD,
#'   or a list of ARDs. Columns are matched by name, and any missing
#'   columns will be filled with `NA.`
#' @param .update (`logical` scalar)\cr
#'   logical indicating whether to update duplicate ARD statistics.
#'   Default is `FALSE`. If a statistic type is repeated and `.update=TRUE`,
#'   the more recently added statistics will be retained, and the others omitted.
#' @param .column (`string`)\cr
#'   string indicating the column that will be returned in the list.
#'   Default is `"statistic"`
#' @param .attributes (`character`)\cr
#'   character vector of column names that will be returned
#'   in the list as attributes.
#'   Default is `NULL`
#'
#' @return a transformed ARD
#' @name ard-helpers
#'
#' @examples
#' ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
#'
#' get_ard_statistics(ard, group1_level %in% "Placebo", variable_level %in% "65-80")
#'
#' bind_ard(ard, ard, .update = TRUE)
NULL

#' @export
#' @rdname ard-helpers
get_ard_statistics <- function(x, ...,
                               .column = "statistic",
                               .attributes = NULL) {
  # subset the ARD
  ard_subset <- dplyr::filter(x, ...)

  # return a named list of the selected stats
  # add attributes for the label, formatting function, warnings, and errors
  seq_len(nrow(ard_subset)) |>
    lapply(
      FUN = function(i) {
        ard_subset[[.column]][[i]] %>%
          {inject(structure(
           ., !!!.create_list_for_attributes(ard_subset, .attributes, i)
          ))}
      }
    ) |>
    stats::setNames(ard_subset[["stat_name"]])
}

.create_list_for_attributes <- function(ard_subset, attributes, i) {
  ret <- list()
  for (attr in seq_along(attributes)) {
    ret <- c(ret, list(ard_subset[[attr]][[i]]))
  }
  stats::setNames(ret, nm = attributes)
}

#' @export
#' @rdname ard-helpers
bind_ard <- function(..., .update = FALSE) {
  # check inputs ---------------------------------------------------------------
  if (!is_scalar_logical(.update))
    cli::cli_abort("Argument {.code .update} must be a class {.cls logical} of length 1.")

  # stack ARDs -----------------------------------------------------------------
  data <- dplyr::bind_rows(...)

  # check for duplicates -------------------------------------------------------
  dupes <-
    dplyr::select(data, all_ard_groups(), all_ard_variables(), "stat_name")[seq(nrow(data), 1L),] |>
    duplicated()

  if (any(dupes) && isTRUE(.update)) {
    cli::cli_inform(c("i" = "{sum(dupes)} duplicate observation{?/s} removed."))
    data <-
      dplyr::filter(
        data,
        .by = c(all_ard_groups(), all_ard_variables(), "stat_name"),
        dplyr::row_number() == dplyr::n()
      )
  }
  else if (any(dupes) && isFALSE(.update)) {
    cli::cli_abort(c("!" = "{sum(dupes)} duplicate observation{?/s} found."))
  }

  # return stacked ARDs --------------------------------------------------------
  tidy_ard_column_order(data)
}


