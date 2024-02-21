#' Bind ARDs
#'
#' Wrapper for `dplyr::bind_rows()` with additional checks
#' for duplicated statistics.
#'
#' @param ... ([`dynamic-dots`][dyn-dots])\cr
#'   ARDs to combine. Each argument can either be an ARD,
#'   or a list of ARDs. Columns are matched by name, and any missing
#'   columns will be filled with `NA`.
#' @param .update (`logical`)\cr
#'   logical indicating whether to update duplicate ARD statistics.
#'   Default is `FALSE`. If a statistic type is repeated and `.update=TRUE`,
#'   the more recently added statistics will be retained, and the others omitted.
#' @param .order (`logical`)\cr
#'   logical indicating whether to order the rows of the stacked ARDs, allowing
#'   statistics that share common group and variable values to appear in
#'   consecutive rows. Default is `FALSE`. Ordering will be based on the order
#'   of the group/variable values prior to stacking.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
#'
#' bind_ard(ard, ard, .update = TRUE)
bind_ard <- function(..., .update = FALSE, .order = FALSE) {
  # check inputs ---------------------------------------------------------------
  check_scalar_logical(.update)
  check_scalar_logical(.order)

  # stack ARDs -----------------------------------------------------------------
  data <- dplyr::bind_rows(...)

  # check for duplicates -------------------------------------------------------
  dupes <-
    dplyr::select(data, all_ard_groups(), all_ard_variables(), "stat_name")[seq(nrow(data), 1L), ] |>
    duplicated()

  if (any(dupes) && isTRUE(.update)) {
    cli::cli_inform(c("i" = "{sum(dupes)} duplicate observation{?/s} removed."))
    data <-
      dplyr::filter(
        data,
        .by = c(all_ard_groups(), all_ard_variables(), "stat_name"),
        dplyr::row_number() == dplyr::n()
      )
  } else if (any(dupes) && isFALSE(.update)) {
    cli::cli_abort(c("!" = "{sum(dupes)} duplicate observation{?/s} found."))
  }


  # optionally reorder ---------------------------------------------------------
  if (isTRUE(.order)) {
    data <- tidy_ard_row_order(data)
  }

  # return stacked ARDs --------------------------------------------------------
  tidy_ard_column_order(data)
}
