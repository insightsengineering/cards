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
#'   Columns `'error'`, `'warning'`, `'context'`, and `'fmt_fun'` *may* be removed
#'   from the print. All other columns will be printed, even if more than `n_col`
#'   columns are present.
#' @param width (`integer`)\cr
#'   width of console output. If `NULL`, will use `getOption("width")`.
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
print.card <- function(x, n = NULL, columns = c("auto", "all"), n_col = 6L, width = NULL, ...) {
  # Basic error checking
  if (!inherits(x, "card")) {
    stop("x must be a card object")
  }
  
  # set console width
  width <- width %||% getOption("width", 80L)
  
  # convert to a data frame so the list columns print the values in the list ---
  x_print <- as.data.frame(x)

  # number of rows to print (modeled after tibbles print) ----------------------
  n <- n %||% ifelse(nrow(x_print) > 20L, 10L, nrow(x_print))
  x_print <- utils::head(x_print, n = n)

  # collect information about column types for better printing
  col_types <- tryCatch(.get_column_types(x_print), error = function(e) NULL)
  
  # remove columns based on width and column threshold -------------------------
  if (match.arg(columns) %in% "auto") {
    x_print <- tryCatch(
      .select_columns_for_printing(x_print, n_col, width),
      error = function(e) {
        # Fallback to original logic if helper function fails
        .select_columns_fallback(x_print, n_col)
      }
    )
  }

  # format columns for better display ------------------------------------------
  x_print <- tryCatch(
    .format_columns_for_printing(x_print),
    error = function(e) {
      # Fallback to basic formatting if helper function fails
      .format_columns_fallback(x_print)
    }
  )

  # final printing --------------------------------------------------------------
  tryCatch(
    .print_header(x, x_print, col_types),
    error = function(e) {
      # Fallback to basic header
      cat("{cards} data frame: ", nrow(x), " x ", ncol(x), "\n", sep = "")
    }
  )
  
  print(x_print)
  
  tryCatch(
    .print_footer(x, x_print, n),
    error = function(e) {
      # Fallback to basic footer
      if (nrow(x) > n) {
        cat("# ... with ", nrow(x) - n, " more rows\n", sep = "")
      }
      if (ncol(x) > ncol(x_print)) {
        missing_cols <- names(x)[!names(x) %in% names(x_print)]
        cat("# ... with ", length(missing_cols), " more variables: ", paste(missing_cols, collapse = ", "), "\n", sep = "")
      }
    }
  )
  
  invisible(x)
}

#' Helper functions for improved print.card() method
#' @noRd
.get_column_types <- function(x) {
  # Get column type information for better display
  col_info <- vapply(x, function(col) {
    if (is.list(col)) {
      first_non_null <- col[!vapply(col, is.null, logical(1))]
      if (length(first_non_null) > 0) {
        paste0("list<", class(first_non_null[[1]])[1], ">")
      } else {
        "list"
      }
    } else {
      class(col)[1]
    }
  }, character(1))
  
  # Create abbreviated type indicators
  type_abbrev <- c(
    "character" = "chr",
    "numeric" = "dbl",
    "integer" = "int",
    "logical" = "lgl",
    "factor" = "fct",
    "Date" = "date",
    "POSIXct" = "dttm",
    "POSIXlt" = "dttm"
  )
  
  # Handle list columns and other types
  for (i in seq_along(col_info)) {
    if (startsWith(col_info[i], "list<")) {
      inner_type <- sub("list<(.*)>", "\\1", col_info[i])
      inner_abbrev <- type_abbrev[inner_type] %||% substr(inner_type, 1, 3)
      col_info[i] <- paste0("list<", inner_abbrev, ">")
    } else {
      col_info[i] <- type_abbrev[col_info[i]] %||% substr(col_info[i], 1, 3)
    }
  }
  
  col_info
}

.select_columns_for_printing <- function(x_print, n_col, width) {
  # Select columns based on importance and available width
  if (exists("all_ard_groups", mode = "function") && exists("all_ard_variables", mode = "function")) {
    x_print <- dplyr::select(
      x_print, all_ard_groups(), all_ard_variables(),
      dplyr::any_of(c(
        "context", "stat_name", "stat_label", "stat", "stat_fmt",
        "fmt_fun", "warning", "error"
      ))
    )
  }
  
  # Remove warning and error columns if nothing to report
  if (ncol(x_print) > n_col && "warning" %in% names(x_print)) {
    if (exists("every", mode = "function")) {
      if (every(x_print[["warning"]], is.null)) {
        x_print[["warning"]] <- NULL
      }
    } else {
      if (all(vapply(x_print[["warning"]], is.null, logical(1)))) {
        x_print[["warning"]] <- NULL
      }
    }
  }
  
  if (ncol(x_print) > n_col && "error" %in% names(x_print)) {
    if (exists("every", mode = "function")) {
      if (every(x_print[["error"]], is.null)) {
        x_print[["error"]] <- NULL
      }
    } else {
      if (all(vapply(x_print[["error"]], is.null, logical(1)))) {
        x_print[["error"]] <- NULL
      }
    }
  }
  
  # Remove less important columns if there are many columns
  if (ncol(x_print) > n_col) {
    x_print[["fmt_fun"]] <- NULL
  }
  if (ncol(x_print) > n_col) {
    x_print[["context"]] <- NULL
  }
  
  x_print
}

# Fallback functions for robustness
.select_columns_fallback <- function(x_print, n_col) {
  # Fallback to original logic if helper functions fail
  if (exists("all_ard_groups", mode = "function") && exists("all_ard_variables", mode = "function")) {
    x_print <- dplyr::select(
      x_print, all_ard_groups(), all_ard_variables(),
      dplyr::any_of(c(
        "context", "stat_name", "stat_label", "stat", "stat_fmt",
        "fmt_fun", "warning", "error"
      ))
    )
  }
  
  # Remove warning and error columns if nothing to report
  if (ncol(x_print) > n_col && "warning" %in% names(x_print)) {
    if (all(vapply(x_print[["warning"]], is.null, logical(1)))) {
      x_print[["warning"]] <- NULL
    }
  }
  if (ncol(x_print) > n_col && "error" %in% names(x_print)) {
    if (all(vapply(x_print[["error"]], is.null, logical(1)))) {
      x_print[["error"]] <- NULL
    }
  }
  
  # Remove less important columns if there are many columns
  if (ncol(x_print) > n_col) {
    x_print[["fmt_fun"]] <- NULL
  }
  if (ncol(x_print) > n_col) {
    x_print[["context"]] <- NULL
  }
  
  x_print
}

.format_columns_for_printing <- function(x_print) {
  # Truncate and format columns for better display
  if (exists("all_ard_groups", mode = "function") && exists("all_ard_variables", mode = "function")) {
    x_print <- tryCatch(
      x_print |>
        dplyr::mutate(
          dplyr::across(
            c(
              all_ard_groups("levels"),
              all_ard_variables("levels"),
              dplyr::any_of(c("context", "stat_label", "warning", "error"))
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
  }
  
  # Format statistics with consistent precision
  if ("stat" %in% names(x_print)) {
    x_print$stat <- lapply(
      x_print$stat,
      function(x) {
        if (is.numeric(x)) {
          if (exists("round5", mode = "function")) {
            res <- round5(x, digits = 3)
          } else {
            res <- round(x, digits = 3)
          }
        } else {
          res <- as.character(x)
        }
        
        if (is.character(res) && nchar(res) > 9) {
          res <- paste0(substr(res, 1, 8), "\u2026")
        }
        res
      }
    )
  }
  
  # Format function columns
  if ("fmt_fun" %in% names(x_print)) {
    x_print$fmt_fun <- lapply(
      x_print$fmt_fun,
      function(x) {
        if (is.function(x)) {
          return("<fn>")
        }
        x
      }
    )
  }
  
  x_print
}

# Fallback for formatting columns
.format_columns_fallback <- function(x_print) {
  # Basic formatting fallback
  
  # Format statistics with consistent precision
  if ("stat" %in% names(x_print)) {
    x_print$stat <- lapply(
      x_print$stat,
      function(x) {
        if (is.numeric(x)) {
          round(x, digits = 3)
        } else {
          as.character(x)
        }
      }
    )
  }
  
  # Format function columns
  if ("fmt_fun" %in% names(x_print)) {
    x_print$fmt_fun <- lapply(
      x_print$fmt_fun,
      function(x) {
        if (is.function(x)) {
          return("<fn>")
        }
        x
      }
    )
  }
  
  x_print
}

.print_header <- function(x, x_print, col_types) {
  # Print header with improved formatting
  if (exists("cli_text", mode = "function", where = "package:cli")) {
    cli::cli_text(cli::col_grey("{{cards}} data frame: {nrow(x)} x {ncol(x)}"))
  } else {
    cat("{cards} data frame: ", nrow(x), " x ", ncol(x), "\n", sep = "")
  }
  
  # Print column types information if helpful
  if (length(col_types) > 0) {
    # Only show column types for printed columns
    printed_col_types <- col_types[names(col_types) %in% names(x_print)]
    if (any(startsWith(printed_col_types, "list"))) {
      list_cols <- names(printed_col_types)[startsWith(printed_col_types, "list")]
      if (length(list_cols) > 0) {
        if (exists("cli_alert_info", mode = "function", where = "package:cli")) {
          cli::cli_alert_info(cli::col_grey(
            "List column{?s}: {paste(list_cols, collapse = ', ')}"
          ))
        } else {
          cat("# List columns: ", paste(list_cols, collapse = ", "), "\n", sep = "")
        }
      }
    }
  }
}

.print_footer <- function(x, x_print, n) {
  # Print footer with navigation help
  if (nrow(x) > n) {
    if (exists("cli_alert_info", mode = "function", where = "package:cli")) {
      cli::cli_alert_info(cli::col_grey("{nrow(x) - n} more rows"))
      cli::cli_alert_info(cli::col_grey("Use {.code print(n = ...)} to see more rows"))
    } else {
      cat("# ... with ", nrow(x) - n, " more rows\n", sep = "")
      cat("# Use print(n = ...) to see more rows\n")
    }
  }
  
  if (ncol(x) > ncol(x_print)) {
    missing_cols <- names(x)[!names(x) %in% names(x_print)]
    if (exists("cli_alert_info", mode = "function", where = "package:cli")) {
      cli::cli_alert_info(cli::col_grey(
        "{length(missing_cols)} more variable{?s}: {paste(missing_cols, collapse = ', ')}"
      ))
    } else {
      cat("# ... with ", length(missing_cols), " more variables: ", paste(missing_cols, collapse = ", "), "\n", sep = "")
    }
  }
}
