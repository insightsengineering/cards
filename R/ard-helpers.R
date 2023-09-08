#' ARD Helpers
#'
#' Helper functions for working with ARD data frames
#'
#' @param x an ARD data frame of class 'card'
#' @param ... optional named arguments indicating rows to subset of the ARD.
#' For example, to return only rows where the column `"AGEGR1"` is `"65-80"`,
#' pass `AGEGR1 = "65-80"`.
#'
#' @return a transformed ARD
#' @name ard-helpers
#'
#' @examples
#' ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
#'
#' filter_ard(ard, group1_level = "Placebo", variable_level = "65-80") |>
#'   flatten_ard()
#'
#' get_ard_statistics(ard, group1_level = "Placebo", variable_level = "65-80")
NULL

#' @export
#' @rdname ard-helpers
get_ard_statistics <- function(x, ...) {
  # subset the ARD
  ard_subset <- filter_ard(x, ...)

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
filter_ard <- function(x, ...) {
  # save dots as named list of expressions -------------------------------------
  dots <- rlang::enexprs(...)

  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "card")) {
    cli::cli_abort("Argument {.code x} must be class {.cls card}.")
  }
  if (!rlang::is_named2(dots)) {
    cli::cli_abort("Arguments passed in {.code ...} must be named.")
  }
  if (rlang::is_empty(dots)) return(x)

  # construct calls to dplyr::filter() -----------------------------------------
  filter_args <-
    .mapply(
      FUN = function(value, colname) {
        if (is.list(x[[colname]])) {
          rlang::expr(
            unlist(lapply(
              X = !!rlang::data_sym(colname),
              FUN = function(p) ifelse(rlang::is_empty(p), FALSE, p %in% !!value)
            ))
          )
        }
        rlang::expr(!!rlang::data_sym(colname) %in% !!value)
      },
      dots = list(dots, names(dots)),
      MoreArgs = NULL
    )

  # filter ard data set and return ---------------------------------------------
  dplyr::filter(x, !!!filter_args)
}


