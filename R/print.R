#' Print
#'
#' `r lifecycle::badge('experimental')`\cr
#' Print method for objects of class 'card'
#'
#' @param x (`data.frame`)\cr
#'   object of class 'card'
#' @param width (`integer`)\cr
#'   integer specifying the width of the console. Default is `getOption("width")`.
#' @param n (`integer`)\cr
#'   integer specifying the number of rows to print
#' @param columns (`string`)\cr
#'   string indicating whether to print a selected number of columns or all.
#' @param n_col (`integer`)\cr
#'   some columns are removed when there are more than a threshold of
#'   columns present. This argument sets that threshold. This is only used
#'   when `columns='auto'` and default is `6L`.
#'   Columns `'error'`, `'warning'`, `'context'`, and `'fmt_fun'` *may* be removed
#'   from the print. All other columns will be printed, even if more than `n_col`
#'   columns are present.
#' @param ... ([`dynamic-dots`][rlang::dyn-dots])\cr
#'   not used
#'
#' @return an ARD data frame of class 'card' (invisibly)
#' @export
#' @keywords internal
#'
#' @examples
#' ard_categorical(ADSL, variables = AGEGR1) |>
#'   print()
print.card <- function(x, width = getOption("width"), n = NULL, columns = c("auto", "all"), n_col = 6L, ...) {
  set_cli_abort_call()

  # assertion ------------------------------------------------------------------
  if (!inherits(x, "card")) {
    cli::cli_abort(c("x" = "{.var x} must be a {.cls card} object."))
  }

  # convert to a data frame so the list columns print the values in the list ---
  x_print <- as.data.frame(x)

  # number of rows to print (modeled after tibbles print) ----------------------
  n <- n %||% ifelse(nrow(x_print) > 20L, 10L, nrow(x_print))
  x_print_n <- utils::head(x_print, n = n)

  # select and format columns --------------------------------------------------
  x_print_n <- .pr_select_cols(x_print_n, columns, n_col)
  x_print_n <- .pr_format_cols(x_print_n)


  # print object ---------------------------------------------------------------
  cli::cli_h1("cards")
  print(x_print_n, right = FALSE)

  # print footer ---------------------------------------------------------------
  if (n < nrow(x)) {
    cli::cli_inform(c("i" = "Showing {n} of {nrow(x)} rows."))
  }

  invisible(x)
}

# helpers ----------------------------------------------------------------------

.pr_select_cols <- function(x, columns, n_col) {
  # remove columns -------------------------------------------------------------
  if (rlang::arg_match(columns, values = c("auto", "all")) %in% "auto") {
    x <-
      dplyr::select(
        x, all_ard_groups(), all_ard_variables(),
        any_of(c(
          "context", "stat_name", "stat_label", "stat", "stat_fmt",
          "fmt_fun", "warning", "error"
        ))
      )

    # remove warning and error columns if nothing to report
    if (ncol(x) > n_col && "warning" %in% names(x) && every(x[["warning"]], is.null)) {
      x[["warning"]] <- NULL
    }
    if (ncol(x) > n_col && "error" %in% names(x) && every(x[["error"]], is.null)) {
      x[["error"]] <- NULL
    }

    # remove 'fmt_fun' col if there are many cols
    if (ncol(x) > n_col) {
      x[["fmt_fun"]] <- NULL
    }
    # remove 'context' col if there are many cols
    if (ncol(x) > n_col) {
      x[["context"]] <- NULL
    }
  }

  x
}

.pr_format_cols <- function(x) {
  # truncate the 'group##_level', 'variable_level', 'stat_label', and 'context' columns ----
  x <-
    tryCatch(
      x |>
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
      error = function(e) x
    )

  # for the statistics, round to 3 decimal places ------------------------------
  if ("stat" %in% names(x)) {
    x$stat <- lapply(
      x$stat,
      function(x) {
        if (isTRUE(is.numeric(x))) {
          res <- round5(x, digits = 3)
        } else {
          res <- x
        }
        res
      }
    )
  }

  # unlist the stat_fmt column so it prints nicely -----------------------------
  if ("stat_fmt" %in% names(x)) {
    x$stat_fmt <- unlist(x$stat_fmt)
  }

  x
}
