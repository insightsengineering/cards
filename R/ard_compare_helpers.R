# Internal helper functions for `ard_compare()`

.format_key_value <- function(value) {
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

.format_duplicate_keys <- function(data, key_columns) {
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
          paste0(column, " = ", .format_key_value(value))
        },
        character(1)
      )
      paste(formatted, collapse = ", ")
    },
    character(1)
  )
}

.duplicate_message <- function(arg_name, data, key_columns, key_origin) {
  key_details <- .format_duplicate_keys(data, key_columns)
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
    call = get_cli_abort_call()
  )
}

.check_key_identify_rows <- function(data, arg_name, key_columns, key_origin) {
  if (anyDuplicated(dplyr::select(data, dplyr::all_of(key_columns))) > 0) {
    .duplicate_message(
      arg_name,
      data,
      key_columns,
      key_origin
    )
  }

  invisible(NULL)
}

.format_keys <- function(data, key_columns, limit = 5L) {
  if (nrow(data) == 0) {
    return(character())
  }

  limited <- head(dplyr::select(data, dplyr::all_of(key_columns)), limit)
  apply(
    limited,
    1,
    function(row) {
      paste(
        vapply(
          seq_along(row),
          function(index) paste0(key_columns[[index]], " = ", .format_key_value(row[[index]])),
          character(1)
        ),
        collapse = ", "
      )
    }
  )
}

.check_rows_not_in_x_y <- function(x_data, y_data, key_columns) {
  x_only <- dplyr::anti_join(x_data, y_data, by = key_columns)
  y_only <- dplyr::anti_join(y_data, x_data, by = key_columns)

  if (nrow(x_only) == 0 && nrow(y_only) == 0) {
    return(invisible(NULL))
  }

  details <- c(
    if (nrow(x_only) > 0) {
      c(
        cli::format_inline("Rows present only in {.arg x}: {nrow(x_only)}."),
        paste0("  - ", .format_keys(x_only, key_columns))
      )
    },
    if (nrow(y_only) > 0) {
      c(
        cli::format_inline("Rows present only in {.arg y}: {nrow(y_only)}."),
        paste0("  - ", .format_keys(y_only, key_columns))
      )
    }
  )

  cli::cli_abort(
    paste(
      c(
        "The input ARDs do not share the same records for the chosen keys.",
        details
      ),
      collapse = "\n"
    ),
    call = get_cli_abort_call()
  )
}

.ensure_column <- function(data, column) {
  if (!column %in% names(data)) {
    data[[column]] <- vector("list", nrow(data))
  }
  data
}

.build_mismatches <- function(comparison, column, key_columns) {
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
