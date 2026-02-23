#' Process keys Argument
#'
#' @param x (`card`)\cr first ARD
#' @param y (`card`)\cr second ARD
#' @param keys tidyselect expression for key columns
#'
#' @return character vector of key column names
#' @keywords internal
#' @noRd
.process_keys_arg <- function(x, y, keys) {
  keys <- intersect(
    cards_select({{ keys }}, data = x),
    cards_select({{ keys }}, data = y)
  )
  .check_not_empty(keys)
  cli::cli_inform("The comparison {.arg keys} are {.val {keys}}.")
  keys
}

#' Process compare Argument
#'
#' @param x (`card`)\cr first ARD
#' @param y (`card`)\cr second ARD
#' @param compare tidyselect expression for columns to compare
#'
#' @return character vector of column names to compare
#' @keywords internal
#' @noRd
.process_compare_arg <- function(x, y, compare) {
  compare <- union(
    cards_select({{ compare }}, data = x),
    cards_select({{ compare }}, data = y)
  )
  .check_not_empty(compare)
  cli::cli_inform("The comparison {.arg compare} columns are {.val {compare}}.")
  compare
}

#' Check Argument is Not Empty
#'
#' @param x object to check
#' @param arg_name name of argument for error message
#'
#' @return invisible x
#' @keywords internal
#' @noRd
.check_not_empty <- function(x, arg_name = rlang::caller_arg(x)) {
  if (rlang::is_empty(x)) {
    cli::cli_abort(
      "The {.arg {arg_name}} argument cannot be empty.",
      call = get_cli_abort_call()
    )
  }
  invisible(x)
}

#' Check Keys Uniquely Identify Rows
#'
#' @param data data frame to check
#' @param keys character vector of key column names
#' @param arg_name name of argument for error message
#'
#' @return invisible NULL
#' @keywords internal
#' @noRd
.check_keys_unique <- function(data, keys, arg_name) {
  if (anyDuplicated(dplyr::select(data, dplyr::all_of(keys))) > 0) {
    duplicated_keys <- .format_duplicate_keys(data, keys)

    cli::cli_abort(
      c(
        "!" = "Duplicate key combinations detected in {.arg {arg_name}}.",
        "i" = "Key columns: {.val {keys}}.",
        duplicated_keys
      ),
      call = get_cli_abort_call()
    )
  }

  invisible(NULL)
}

#' Format Duplicate Keys for Error Message
#'
#' @param data data frame
#' @param keys character vector of key column names
#'
#' @return character vector of formatted duplicate key descriptions
#' @keywords internal
#' @noRd
.format_duplicate_keys <- function(data, keys, limit = 5L) {
  key_data <- dplyr::select(data, dplyr::all_of(keys))
  duplicated_rows <- duplicated(key_data) | duplicated(key_data, fromLast = TRUE)

  if (!any(duplicated_rows)) {
    return(character())
  }

  unique_duplicates <- utils::head(unique(key_data[duplicated_rows, , drop = FALSE]), limit)

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
  ) |>
    paste0("- ", x = _)
}

#' Format a Single Key Value
#'
#' @param value value to format
#'
#' @return formatted string
#' @keywords internal
#' @noRd
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

#' Compare Rows Between Two ARDs
#'
#' Returns rows present in x but not in y based on key columns.
#'
#' @param x (`card`)\cr first ARD
#' @param y (`card`)\cr second ARD
#' @param keys character vector of key column names
#'
#' @return data frame of rows in x not in y
#' @keywords internal
#' @noRd
.compare_rows <- function(x, y, keys) {
  dplyr::anti_join(
    dplyr::select(x, dplyr::all_of(keys)),
    dplyr::select(y, dplyr::all_of(keys)),
    by = keys
  )
}

#' Compare Columns Between Two ARDs
#'
#' Loops through columns to compare and returns a named list of data frames
#' where each data frame contains rows that are not equal between x and y.
#'
#' @param x (`card`)\cr first ARD
#' @param y (`card`)\cr second ARD
#' @param keys character vector of key column names
#' @param compare character vector of column names to compare
#'
#' @return named list of data frames with mismatched rows
#' @keywords internal
#' @noRd
.compare_columns <- function(x, y, keys, compare, tolerance, check.attributes) {
  # select relevant columns
  x_selected <- dplyr::select(x, dplyr::all_of(keys), dplyr::any_of(compare))
  y_selected <- dplyr::select(y, dplyr::all_of(keys), dplyr::any_of(compare))

  # ensure all compare columns exist in both data frames
  for (column in compare) {
    if (!column %in% names(x_selected)) {
      x_selected[[column]] <- vector("list", nrow(x_selected))
    }
    if (!column %in% names(y_selected)) {
      y_selected[[column]] <- vector("list", nrow(y_selected))
    }
  }

  # perform inner join to compare only matching rows
  # perform inner join to compare only matching rows
  comparison <- dplyr::inner_join(
    x_selected,
    y_selected,
    by = keys,
    suffix = c(".x", ".y")
  ) |> 
    unlist_ard_columns()
  
  # Remove "cards" class from the object
  class(comparison) <- setdiff(class(comparison), "card")
  
  # build mismatch data frame for each compare column
  lapply(
    stats::setNames(compare, compare),
    function(column) {
      column_x <- paste0(column, ".x")
      column_y <- paste0(column, ".y")

      # find rows where values differ
      diffs <- mapply(
        all.equal,
        comparison[[column_x]],
        comparison[[column_y]],
        MoreArgs = list(tolerance = tolerance, check.attributes = check.attributes),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )

      is_equal <- vapply(diffs, isTRUE, logical(1))
      mismatches <- comparison[!is_equal, , drop = FALSE]
      mismatches$difference <- diffs[!is_equal]

      dplyr::select(
        mismatches,
        dplyr::all_of(c(keys, column_x, column_y)),
        "difference"
      )
    }
  )
}

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

  limited <- utils::head(dplyr::select(data, dplyr::all_of(key_columns)), limit)
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
