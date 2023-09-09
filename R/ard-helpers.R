#' ARD Helpers
#'
#' Helper functions for working with ARD data frames.
#'
#' - `get_ard_statistics()`: This function returns the statistics from an ARD
#'   as a named list. The statistic label, formatting instruction, and any error
#'   or warning messages are returned as attributes.
#'
#' @param x an ARD data frame of class 'card'
#' @param ... optional named arguments indicating rows to subset of the ARD.
#' For example, to return only rows where the column `"AGEGR1"` is `"65-80"`,
#' pass `AGEGR1 %in% "65-80"`.
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

