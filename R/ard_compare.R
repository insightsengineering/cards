#' Compare ARDs
#'
#' @include import-standalone-checks.R
#'
#' @description
#' `ard_compare()` compares the `stat`, `fmt_fun`, `warning`, and `error`
#' columns of two ARDs row-by-row using a shared set of key columns. Rows where
#' the column values differ are returned.
#'
#' @param x (`card`)\cr
#'   first ARD to compare.
#' @param y (`card`)\cr
#'   second ARD to compare.
#' @param key_columns (`character` or [`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   optional specification of column names identifying unique records. Supply a
#'   character vector or tidyselect expression. When `NULL`, grouping columns
#'   along with `"variable"`, `"variable_level"`, and `"stat_name"` are used.
#'   If the ARDs do not have identical primary key sets, the comparison falls
#'   back to the shared primary key columns provided they uniquely identify rows
#'   in both inputs.
#'
#' @return a named list of data frames containing key columns with the
#'   corresponding values from both ARDs for rows where the column values do not
#'   match. The list contains entries for the `stat`, `fmt_fun`, `warning`, and
#'   `error` columns (the `fmt_fun` component is labeled `fmt_fn` for
#'   compatibility with previous naming conventions).
#'
#' @details
#' When both ARDs retain metadata about the environment that created them (for
#' example via an attribute storing an environment object), the function ensures
#' that the environments match before comparing values.
#' @importFrom rlang env_label
#' @export
#'
#' @examples
#' ard_base <- ard_summary(ADSL, variables = AGE)
#' ard_modified <- ard_summary(dplyr::mutate(ADSL, AGE = AGE + 1), variables = AGE)
#'
#' ard_compare(ard_base, ard_modified)$stat
#'
ard_compare <- function(x, y, key_columns = NULL) {
  set_cli_abort_call()

  check_class(x, cls = "card")
  check_class(y, cls = "card")

  .validate_environment_metadata(x, y, call = get_cli_abort_call())

  primary_x <-
    dplyr::select(x, all_ard_groups(), dplyr::any_of(c("variable", "variable_level", "stat_name"))) |>
    names()
  primary_y <-
    dplyr::select(y, all_ard_groups(), dplyr::any_of(c("variable", "variable_level", "stat_name"))) |>
    names()

  same_primary <- setequal(primary_x, primary_y)
  primary_intersection <- intersect(primary_x, primary_y)
  key_origin <- "user"
  key_columns_quo <- rlang::enquo(key_columns)
  use_default_keys <-
    rlang::quo_is_missing(key_columns_quo) || rlang::quo_is_null(key_columns_quo)

  if (use_default_keys) {
    if (same_primary) {
      key_columns <- primary_x
      key_origin <- "primary"
    } else {
      if (rlang::is_empty(primary_intersection)) {
        cli::cli_abort(
          c(
            "!" = "The input ARDs do not share any primary key columns.",
            "x" = "Primary key columns in {.arg x}: {.val {primary_x}}.",
            "x" = "Primary key columns in {.arg y}: {.val {primary_y}}."
          ),
          call = get_cli_abort_call()
        )
      }
      key_columns <- primary_intersection
      key_origin <- "intersection"
    }
  } else {
    key_columns_expr <- rlang::get_expr(key_columns_quo)
    key_columns_env <- rlang::quo_get_env(key_columns_quo)
    key_columns_value <- tryCatch(
      rlang::eval_tidy(
        key_columns_expr,
        data = NULL,
        env = key_columns_env
      ),
      error = identity
    )

    if (!inherits(key_columns_value, "error") && is.character(key_columns_value)) {
      key_columns <- key_columns_value
    } else {
      key_columns <- NULL
      process_selectors(x, key_columns = !!key_columns_quo, env = environment())
    }

    if (!is.character(key_columns)) {
      cli::cli_abort(
        "The {.arg key_columns} argument must be a character vector.",
        call = get_cli_abort_call()
      )
    }

    key_columns <- unique(key_columns)
    key_origin <- "user"
  }

  key_columns <- unique(key_columns)

  if (rlang::is_empty(key_columns)) {
    cli::cli_abort(
      "At least one column must be supplied in {.arg key_columns}.",
      call = get_cli_abort_call()
    )
  }

  missing_x <- setdiff(key_columns, names(x))
  missing_y <- setdiff(key_columns, names(y))
  if (!rlang::is_empty(missing_x) || !rlang::is_empty(missing_y)) {
    cli::cli_abort(
      c(
        "!" = "The provided {.arg key_columns} must exist in both ARDs.",
        if (!rlang::is_empty(missing_x)) {
          "x" <- "Missing in {.arg x}: {.val {missing_x}}."
        },
        if (!rlang::is_empty(missing_y)) {
          "x" <- "Missing in {.arg y}: {.val {missing_y}}."
        }
      ),
      call = get_cli_abort_call()
    )
  }

  format_key_value <- function(value) {
    value <- value[[1]]

    if (is.factor(value)) {
      value <- as.character(value)
    }

    if (is.character(value)) {
      if (is.na(value)) {
        return("NA")
      }
      return(encodeString(value, quote = "\""))
    }

    if (is.logical(value)) {
      if (is.na(value)) {
        return("NA")
      }
      return(if (value) "TRUE" else "FALSE")
    }

    if (is.numeric(value)) {
      if (is.na(value)) {
        return("NA")
      }
      return(format(value, trim = TRUE))
    }

    if (inherits(value, "Date") || inherits(value, "POSIXt")) {
      if (is.na(value)) {
        return("NA")
      }
      return(encodeString(as.character(value), quote = "\""))
    }

    value_chr <- as.character(value)
    if (length(value_chr) == 0 || is.na(value_chr)) {
      return("NA")
    }

    encodeString(value_chr, quote = "\"")
  }

  format_duplicate_keys <- function(data) {
    key_data <- dplyr::select(data, dplyr::all_of(key_columns))
    duplicated_rows <- duplicated(key_data) | duplicated(key_data, fromLast = TRUE)

    if (!any(duplicated_rows)) {
      return(character())
    }

    unique_duplicates <- unique(key_data[duplicated_rows, , drop = FALSE])

    vapply(
      seq_len(nrow(unique_duplicates)),
      function(row_index) {
        row <- unique_duplicates[row_index, , drop = FALSE]
        formatted <- vapply(
          names(row),
          function(column) {
            value <- row[[column]]
            paste0(column, " = ", format_key_value(value))
          },
          character(1)
        )
        paste(formatted, collapse = ", ")
      },
      character(1)
    )
  }

  duplicate_message <- function(arg_name, data) {
    key_details <- format_duplicate_keys(data)
    # duplicate_message <- function(arg_name) {
    duplicate_header <- switch(key_origin,
      intersection = cli::format_inline(
        "The shared primary key columns do not uniquely identify rows in {.arg {arg_name}}.",
        arg_name = arg_name
      ),
      cli::format_inline(
        "Duplicate key combinations detected in {.arg {arg_name}}.",
        arg_name = arg_name
      )
    )

    column_detail <- switch(key_origin,
      intersection = cli::format_inline(
        "Columns used: {.val {key_columns}}.",
        key_columns = key_columns
      ),
      user = cli::format_inline(
        "Columns supplied via {.arg key_columns}: {.val {key_columns}}.",
        key_columns = key_columns
      ),
      cli::format_inline(
        "Columns used: {.val {key_columns}}.",
        key_columns = key_columns
      )
    )

    detail_lines <- if (length(key_details) > 0) {
      paste0("  - ", key_details)
    } else {
      character()
    }

    cli::cli_abort(
      paste(c(
        duplicate_header,
        column_detail,
        detail_lines
      ), collapse = "\n"),
      # c(
      #   "!" = duplicate_header,
      #   "i" = column_detail
      # ),
      call = get_cli_abort_call()
    )
  }

  if (anyDuplicated(dplyr::select(x, dplyr::all_of(key_columns))) > 0) {
    duplicate_message(
      "x",
      x
    )
  }
  if (anyDuplicated(dplyr::select(y, dplyr::all_of(key_columns))) > 0) {
    duplicate_message(
      "y",
      y
    )
  }

  fmt_column <- if ("fmt_fun" %in% names(x) || "fmt_fun" %in% names(y)) {
    "fmt_fun"
  } else if ("fmt_fn" %in% names(x) || "fmt_fn" %in% names(y)) {
    "fmt_fn"
  } else {
    "fmt_fun"
  }

  comparison_targets <- list(
    stat = "stat",
    fmt_fn = fmt_column,
    warning = "warning",
    error = "error"
  )

  comparison_columns <- unique(unlist(comparison_targets, use.names = FALSE))

  x_selected <-
    dplyr::select(
      x,
      dplyr::all_of(key_columns),
      dplyr::any_of(comparison_columns)
    )
  y_selected <-
    dplyr::select(
      y,
      dplyr::all_of(key_columns),
      dplyr::any_of(comparison_columns)
    )

  ensure_column <- function(data, column) {
    if (!column %in% names(data)) {
      data[[column]] <- vector("list", nrow(data))
    }
    data
  }

  for (column in comparison_columns) {
    x_selected <- ensure_column(x_selected, column)
    y_selected <- ensure_column(y_selected, column)
  }

  comparison <-
    dplyr::full_join(
      x_selected,
      y_selected,
      by = key_columns,
      suffix = c(".x", ".y")
    )

  build_mismatches <- function(column) {
    column_x <- paste0(column, ".x")
    column_y <- paste0(column, ".y")

    if (!all(c(column_x, column_y) %in% names(comparison))) {
      empty <- comparison[0, , drop = FALSE]
      missing_cols <- setdiff(c(column_x, column_y), names(empty))
      for (missing_col in missing_cols) {
        empty[[missing_col]] <- vector("list", 0)
      }
      return(
        dplyr::select(
          empty,
          dplyr::all_of(c(key_columns, column_x, column_y))
        )
      )
    }

    matches <- mapply(
      identical,
      comparison[[column_x]],
      comparison[[column_y]],
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

    mismatches <- comparison[!matches, , drop = FALSE]

    dplyr::select(
      mismatches,
      dplyr::all_of(c(key_columns, column_x, column_y))
    )
  }

  mismatch_list <- lapply(comparison_targets, build_mismatches)

  names(mismatch_list) <- names(comparison_targets)

  mismatch_list
}
