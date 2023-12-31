#' Print
#'
#' Print method for objects of class 'card'
#'
#' @param x object of class 'card'
#' @param n integer specifying the number of rows to print
#' @param columns string indicating whether to print a selected number of
#' columns or all.
#' @param n_col some columns are removed when there are more than a threshold
#' of columns present. This argument sets that threshold. Default is `6L`
#' @param ... not used
#'
#' @return data frame
#' @export
#'
#' @examples
#' ard_categorical(ADSL, variables = AGEGR1) |>
#'   print()
print.card <- function(x, n = NULL, columns = c("auto", "all"), n_col = 6L, ...) {
  # convert to a data frame so the list columns print the values in the list ---
  x_print <- as.data.frame(x)

  # number of rows to print (modeled after tibbles print) ----------------------
  n <- n %||% ifelse(nrow(x_print) > 20L, 10L, nrow(x_print))
  x_print <- utils::head(x_print, n = n)

  # remove columns -------------------------------------------------------------
  if (arg_match(columns) %in% "auto") {
    # remove warning and error columns if nothing to report
    if ("error" %in% names(x_print) && every(x_print[["error"]], is.null))
      x_print[["error"]] <- NULL
    if ("warning" %in% names(x_print) && every(x_print[["warning"]], is.null))
      x_print[["warning"]] <- NULL
    if (ncol(x_print) > n_col)
      x_print[["statistic_fmt_fn"]] <- NULL # remove this col if there are many cols
    if (ncol(x_print) > n_col)
      x_print[["context"]] <- NULL # remove this col if there are many cols
  }

  # truncate the 'group##_level', 'variable_level', 'stat_label', and 'context' columns ------
  x_print <-
    tryCatch(
      x_print |>
        dplyr::mutate(
          across(
            c(all_ard_groups(FALSE, TRUE),
              all_ard_variables(FALSE, TRUE),
              any_of(c("context", "stat_label", "warning", "error"))),
            function(x) {
              lapply(
                x,
                function(e) {
                  e <- as.character(e) |> paste(collapse = ", ")
                  ifelse(nchar(e) > 9, paste0(substr(e, 1, 8), "\u2026"), e)
                }
              )
            }
          )
        ),
      error = function(e) x_print
    )

  # for the statistics, round to 3 decimal places ------------------------------
  if ("statistic" %in% names(x_print)) {
    x_print$statistic <- lapply(
      x_print$statistic,
      function(x) {
        if (isTRUE(is.double(x))) return(round5(x, digits = 3))
        if (is_string(x) && nchar(x) > 9) return(paste0(substr(x, 1, 8), "\u2026"))
        x
      }
    )
  }

  # for the formatting function column, abbreviate the print of proper functions
  if ("statistic_fmt_fn" %in% names(x_print)) {
    x_print$statistic_fmt_fn <- lapply(
      x_print$statistic_fmt_fn,
      function(x) {
        if (isTRUE(is.function(x))) {
          return("<fn>")
        }
        x
      }
    )
  }

 # final printing --------------------------------------------------------------
  cli::cli_text(cli::col_grey("{{cards}} data frame: {nrow(x)} x {ncol(x)}"))
  print(x_print)
  if (nrow(x) > n) {
    cli::cli_alert_info(cli::col_grey("{nrow(x) - n} more rows"))
    cli::cli_alert_info(cli::col_grey("Use {.code print(n = ...)} to see more rows"))
  }
  if (ncol(x) > ncol(x_print)) {
    missing_cols <- names(x) |> setdiff(names(x_print))
    cli::cli_alert_info(cli::col_grey(
      "{length(missing_cols)} more variable{?s}: {paste(missing_cols, collapse = ', ')}"
    ))
  }
  invisible(x)
}
