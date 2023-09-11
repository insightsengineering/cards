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
#' - `all_ard_groups()`: Use this function in dplyr selecting environments, such
#'   as `dplyr::select()`. Function selects grouping columns, e.g. columns
#'   named `"group##"` or `"group##_level"`.
#'
#' - `all_ard_variables()`: Use this function in dplyr selecting environments, such
#'   as `dplyr::select()`. Function selects variables columns, e.g. columns
#'   named `"variable"` or `"variable_level"`.
#'
#' @param x an ARD data frame of class 'card'
#' @param ...
#' - `get_ard_statistics()`: optional named arguments indicating rows to subset of the ARD.
#'   For example, to return only rows where the column `"AGEGR1"` is `"65-80"`,
#'   pass `AGEGR1 %in% "65-80"`.
#' - `bind_ard()`: ARDs to combine. Each argument can either be an ARD,
#'   or a list of ARDs. Columns are matched by name, and any missing
#'   columns will be filled with `NA.`
#' @param .update logical indicating whether to update duplicate ARD statistics.
#'   Default is `FALSE`. If a statistic type is repeated and `.update=TRUE`,
#'   the more recently added statistics will be retained, and the others omitted.
#'
#' @return a transformed ARD
#' @name ard-helpers
#'
#' @examples
#' ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
#'
#' get_ard_statistics(ard, group1_level %in% "Placebo", variable_level %in% "65-80")
NULL

#' @export
#' @rdname ard-helpers
get_ard_statistics <- function(x, ...) {
  # subset the ARD
  ard_subset <- dplyr::filter(x, ...)

  # return a named list of the selected stats
  # add attributes for the label, formatting function, warnings, and errors
  seq_len(nrow(ard_subset)) |>
    lapply(
      FUN = function(i) {
        ard_subset$statistic[[i]] |>
          structure(
            stat_label = ard_subset[["stat_label"]][[i]],
            statistic_fmt_fn = ard_subset[["statistic_fmt_fn"]][[i]],
            warning = ard_subset[["warning"]][[i]],
            error = ard_subset[["error"]][[i]]
          )
      }
    ) |>
    stats::setNames(ard_subset[["stat_name"]])
}

#' @export
#' @rdname ard-helpers
bind_ard <- function(..., .update = FALSE) {
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
  data
}

#' @export
#' @rdname ard-helpers
all_ard_groups <- function() {
  dplyr::matches("^group[0-9]+$|^group[0-9]+_level$")
}

#' @export
#' @rdname ard-helpers
all_ard_variables <- function() {
  dplyr::any_of(c("variable", "variable_level"))
}
