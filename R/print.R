#' Print
#'
#' `r lifecycle::badge('experimental')`\cr
#' Print method for objects of class 'card'
#'
#' @param x (`data.frame`)\cr
#'   object of class 'card'
#' @param n (`integer`)\cr
#'   integer specifying the number of rows to print
#' @param columns (`string`)\cr
#'   string indicating whether to print a selected number of columns or all.
#' @param n_col (`integer`)\cr
#'   some columns are removed when there are more than a threshold of
#'   columns present. This argument sets that threshold. This is only used
#'   when `columns='auto'` and default is `6L`.
#'   Columns `'error'`, `'warning'`, `'context'`, and `'fmt_fn'` *may* be removed
#'   from the print. All other columns will be printed, even if more than `n_col`
#'   columns are present.
#' @param ... ([`dynamic-dots`][rlang::dyn-dots])\cr
#'   not used
#'
#' @return an ARD data frame of class 'card' (invisibly)
#' @export
#'
#' @examples
#' ard_categorical(ADSL, variables = AGEGR1) |>
#'   print()
print.card <- function(x, n = NULL, columns = c("auto", "all"), n_col = 6L, ...) {
  set_cli_abort_call()

  # convert to a data frame so the list columns print the values in the list ---
  x_print <- as.data.frame(x)

  # number of rows to print (modeled after tibbles print) ----------------------
  n <- n %||% ifelse(nrow(x_print) > 20L, 10L, nrow(x_print))
  x_print <- utils::head(x_print, n = n)

  # remove columns -------------------------------------------------------------
  if (arg_match(columns) %in% "auto") {
    # remove warning and error columns if nothing to report
    if (ncol(x_print) > n_col && "warning" %in% names(x_print) && every(x_print[["warning"]], is.null)) {
      x_print[["warning"]] <- NULL
    }
    if (ncol(x_print) > n_col && "error" %in% names(x_print) && every(x_print[["error"]], is.null)) {
      x_print[["error"]] <- NULL
    }

    # remove 'fmt_fn' col if there are many cols
    if (ncol(x_print) > n_col) {
      x_print[["fmt_fn"]] <- NULL
    }
    # remove 'context' col if there are many cols
    if (ncol(x_print) > n_col) {
      x_print[["context"]] <- NULL
    }
  }

  # truncate the 'group##_level', 'variable_level', 'stat_label', and 'context' columns ------
  x_print <-
    tryCatch(
      x_print |>
        dplyr::mutate(
          across(
            c(
              all_ard_groups("levels"),
              all_ard_variables("levels"),
              any_of(c("context", "stat_label", "warning", "error"))
            ),
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
  if ("stat" %in% names(x_print)) {
    x_print$stat <- lapply(
      x_print$stat,
      function(x) {
        if (isTRUE(is.numeric(x))) {
          res <- round5(x, digits = 3)
        } else {
          res <- as.character(x)
        }

        if (is_string(res) && nchar(res) > 9) {
          res <- paste0(substr(res, 1, 8), "\u2026")
        }
        res
      }
    )
  }

  # for the formatting function column, abbreviate the print of proper functions
  if ("fmt_fn" %in% names(x_print)) {
    x_print$fmt_fn <- lapply(
      x_print$fmt_fn,
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
