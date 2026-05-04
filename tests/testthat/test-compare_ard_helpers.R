# --- .process_keys_arg() ------------------------------------------------------

test_that(".process_keys_arg() returns key column names", {
  ard <- ard_summary(ADSL, variables = AGE)

  result <- .process_keys_arg(ard, ard, keys = c("variable", "stat_name"))
  expect_equal(result, c("variable", "stat_name"))
})

test_that(".process_keys_arg() errors when keys mismatch between x and y", {
  ard1 <- ard_summary(ADSL, variables = AGE)
  ard2 <- ard1 |> dplyr::select(-context)

  expect_error(
    .process_keys_arg(ard1, ard2, keys = c("variable", "context"))
  )
})

# --- .process_compare_arg() ---------------------------------------------------

test_that(".process_compare_arg() returns column names", {
  ard <- ard_summary(ADSL, variables = AGE)

  result <- .process_compare_arg(ard, ard, columns = "stat")
  expect_equal(result, "stat")
})

test_that(".process_compare_arg() errors when columns mismatch", {
  ard1 <- ard_summary(ADSL, variables = AGE)
  ard2 <- ard1 |> dplyr::select(-stat_label)

  expect_error(
    .process_compare_arg(ard1, ard2, columns = c("stat", "stat_label"))
  )
})

# --- .check_not_empty() -------------------------------------------------------

test_that(".check_not_empty() passes for non-empty input", {
  expect_invisible(.check_not_empty(c("a", "b")))
})

test_that(".check_not_empty() errors for empty input", {
  expect_error(
    .check_not_empty(character()),
    "cannot be empty"
  )
})

# --- .check_keys_unique() -----------------------------------------------------

test_that(".check_keys_unique() passes for unique keys", {
  ard <- ard_summary(ADSL, variables = AGE)
  expect_invisible(
    .check_keys_unique(ard, c("variable", "stat_name"), arg_name = "x")
  )
})

test_that(".check_keys_unique() errors for duplicate keys", {
  ard <- ard_summary(ADSL, variables = AGE)
  ard_dup <- dplyr::bind_rows(ard, ard)

  expect_error(
    .check_keys_unique(ard_dup, c("variable", "stat_name"), arg_name = "x"),
    "Duplicate key combinations"
  )
})

# --- .format_duplicate_keys() -------------------------------------------------

test_that(".format_duplicate_keys() formats duplicates", {
  ard <- ard_summary(ADSL, variables = AGE)
  ard_dup <- dplyr::bind_rows(ard, ard)

  result <- .format_duplicate_keys(ard_dup, c("variable", "stat_name"))

  expect_type(result, "character")
  expect_gt(length(result), 0L)
  expect_true(all(grepl("variable", result)))
  expect_true(all(grepl("stat_name", result)))
})

test_that(".format_duplicate_keys() returns empty for no duplicates", {
  ard <- ard_summary(ADSL, variables = AGE)

  result <- .format_duplicate_keys(ard, c("variable", "stat_name"))
  expect_equal(result, character())
})

# --- .format_key_value() ------------------------------------------------------

test_that(".format_key_value() formats character values", {
  expect_equal(.format_key_value("hello"), "\"hello\"")
})

test_that(".format_key_value() formats NA character", {
  expect_equal(.format_key_value(NA_character_), "NA")
})

test_that(".format_key_value() formats logical values", {
  expect_equal(.format_key_value(TRUE), "TRUE")
  expect_equal(.format_key_value(FALSE), "FALSE")
})

test_that(".format_key_value() formats NA logical", {
  expect_equal(.format_key_value(NA), "NA")
})

test_that(".format_key_value() formats numeric values", {
  expect_equal(.format_key_value(42), "42")
  expect_equal(.format_key_value(3.14), "3.14")
})

test_that(".format_key_value() formats NA numeric", {
  expect_equal(.format_key_value(NA_real_), "NA")
})

test_that(".format_key_value() formats factor values", {
  expect_equal(.format_key_value(factor("level_a")), "\"level_a\"")
})

test_that(".format_key_value() formats Date values", {
  expect_equal(.format_key_value(as.Date("2024-01-15")), "\"2024-01-15\"")
})

test_that(".format_key_value() formats NA Date", {
  expect_equal(.format_key_value(as.Date(NA)), "NA")
})

test_that(".format_key_value() formats POSIXt values", {
  dt <- as.POSIXct("2024-01-15 10:30:00", tz = "UTC")
  result <- .format_key_value(dt)
  expect_true(grepl("2024-01-15", result))
})

test_that(".format_key_value() formats NA POSIXt", {
  expect_equal(.format_key_value(as.POSIXct(NA)), "NA")
})

test_that(".format_key_value() handles unknown types via as.character fallback", {
  # raw type falls through all type checks to the as.character fallback
  expect_type(.format_key_value(as.raw(0x1)), "character")
})

# --- .compare_rows() ---------------------------------------------------------

test_that(".compare_rows() returns rows in x not in y", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_subset <- ard_base |> dplyr::filter(stat_name != "mean")
  keys <- c("variable", "context", "stat_name")

  result <- .compare_rows(ard_base, ard_subset, keys)
  expect_equal(nrow(result), 1L)
  expect_equal(result$stat_name, "mean")
})

test_that(".compare_rows() returns empty when all rows match", {
  ard <- ard_summary(ADSL, variables = AGE)
  keys <- c("variable", "context", "stat_name")

  result <- .compare_rows(ard, ard, keys)
  expect_equal(nrow(result), 0L)
})

# --- .compare_columns() ------------------------------------------------------

test_that(".compare_columns() detects value differences", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_base
  ard_modified$stat[1] <- list(999L)
  keys <- c("variable", "context", "stat_name")

  result <- .compare_columns(
    ard_base, ard_modified, keys, compare = "stat",
    tolerance = sqrt(.Machine$double.eps), check.attributes = TRUE
  )

  expect_true("stat" %in% names(result))
  expect_equal(nrow(result$stat), 1L)
  expect_true("difference" %in% names(result$stat))
})

test_that(".compare_columns() returns empty for identical data", {
  ard <- ard_summary(ADSL, variables = AGE)
  keys <- c("variable", "context", "stat_name")

  result <- .compare_columns(
    ard, ard, keys, compare = "stat",
    tolerance = sqrt(.Machine$double.eps), check.attributes = TRUE
  )

  expect_equal(nrow(result$stat), 0L)
})

test_that(".compare_columns() respects tolerance for numeric comparison", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_base
  mean_idx <- which(ard_modified$stat_name == "mean")
  ard_modified$stat[[mean_idx]] <- ard_modified$stat[[mean_idx]] + 1e-10
  keys <- c("variable", "context", "stat_name")

  # default tolerance ignores tiny diff
  result_default <- .compare_columns(
    ard_base, ard_modified, keys, compare = "stat",
    tolerance = sqrt(.Machine$double.eps), check.attributes = TRUE
  )
  expect_equal(nrow(result_default$stat), 0L)

  # strict tolerance catches it
  result_strict <- .compare_columns(
    ard_base, ard_modified, keys, compare = "stat",
    tolerance = 0, check.attributes = TRUE
  )
  expect_gt(nrow(result_strict$stat), 0L)
})

test_that(".compare_columns() compares multiple columns", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_base
  ard_modified$stat[1] <- list(999L)
  ard_modified$stat_label[1] <- "Changed"
  keys <- c("variable", "context", "stat_name")

  result <- .compare_columns(
    ard_base, ard_modified, keys, compare = c("stat", "stat_label"),
    tolerance = sqrt(.Machine$double.eps), check.attributes = TRUE
  )

  expect_true("stat" %in% names(result))
  expect_true("stat_label" %in% names(result))
  expect_gt(nrow(result$stat), 0L)
  expect_gt(nrow(result$stat_label), 0L)
})

# --- .duplicate_message() -----------------------------------------------------

test_that(".duplicate_message() errors with formatted duplicate info", {
  ard <- ard_summary(ADSL, variables = AGE)
  ard_dup <- dplyr::bind_rows(ard, ard)

  expect_error(
    .duplicate_message("x", ard_dup, c("variable", "stat_name"), "user"),
    "Duplicate key combinations"
  )
})

test_that(".duplicate_message() uses intersection wording", {
  ard <- ard_summary(ADSL, variables = AGE)
  ard_dup <- dplyr::bind_rows(ard, ard)

  expect_error(
    .duplicate_message("x", ard_dup, c("variable", "stat_name"), "intersection"),
    "do not uniquely identify"
  )
})

# --- .check_key_identify_rows() -----------------------------------------------

test_that(".check_key_identify_rows() passes for unique keys", {
  ard <- ard_summary(ADSL, variables = AGE)

  expect_invisible(
    .check_key_identify_rows(ard, "x", c("variable", "stat_name"), "user")
  )
})

test_that(".check_key_identify_rows() errors for duplicate keys", {
  ard <- ard_summary(ADSL, variables = AGE)
  ard_dup <- dplyr::bind_rows(ard, ard)

  expect_error(
    .check_key_identify_rows(ard_dup, "x", c("variable", "stat_name"), "user")
  )
})

# --- .format_keys() -----------------------------------------------------------

test_that(".format_keys() formats key rows", {
  ard <- ard_summary(ADSL, variables = AGE)

  result <- .format_keys(ard, c("variable", "stat_name"), limit = 3L)

  expect_type(result, "character")
  expect_lte(length(result), 3L)
  expect_true(all(grepl("variable", result)))
  expect_true(all(grepl("stat_name", result)))
})

test_that(".format_keys() returns empty for zero-row data", {
  ard <- ard_summary(ADSL, variables = AGE)
  empty <- ard[0, ]

  result <- .format_keys(empty, c("variable", "stat_name"))
  expect_equal(result, character())
})

# --- .check_rows_not_in_x_y() ------------------------------------------------

test_that(".check_rows_not_in_x_y() passes when all rows match", {
  ard <- ard_summary(ADSL, variables = AGE)
  keys <- c("variable", "context", "stat_name")

  expect_invisible(
    .check_rows_not_in_x_y(ard, ard, keys)
  )
})

test_that(".check_rows_not_in_x_y() errors when rows differ", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_subset <- ard_base |> dplyr::filter(stat_name != "mean")
  keys <- c("variable", "context", "stat_name")

  expect_error(
    .check_rows_not_in_x_y(ard_base, ard_subset, keys),
    "do not share the same records"
  )
})

# --- .ensure_column() ---------------------------------------------------------

test_that(".ensure_column() adds missing column", {
  df <- data.frame(a = 1:3)

  result <- .ensure_column(df, "b")
  expect_true("b" %in% names(result))
  expect_type(result$b, "list")
  expect_equal(length(result$b), 3L)
})

test_that(".ensure_column() preserves existing column", {
  df <- data.frame(a = 1:3, b = 4:6)

  result <- .ensure_column(df, "b")
  expect_equal(result$b, 4:6)
})

# --- .build_mismatches() ------------------------------------------------------

test_that(".build_mismatches() finds mismatched rows", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_base
  ard_modified$stat[1] <- list(999L)
  keys <- c("variable", "context", "stat_name")

  comparison <- dplyr::inner_join(
    ard_base[c(keys, "stat")],
    ard_modified[c(keys, "stat")],
    by = keys,
    suffix = c(".x", ".y")
  )

  result <- .build_mismatches(comparison, "stat", keys)
  expect_gt(nrow(result), 0L)
  expect_true(all(c("stat.x", "stat.y") %in% names(result)))
})

test_that(".build_mismatches() returns empty for identical data", {
  ard <- ard_summary(ADSL, variables = AGE)
  keys <- c("variable", "context", "stat_name")

  comparison <- dplyr::inner_join(
    ard[c(keys, "stat")],
    ard[c(keys, "stat")],
    by = keys,
    suffix = c(".x", ".y")
  )

  result <- .build_mismatches(comparison, "stat", keys)
  expect_equal(nrow(result), 0L)
})

test_that(".build_mismatches() handles missing comparison columns", {
  ard <- ard_summary(ADSL, variables = AGE)
  keys <- c("variable", "context", "stat_name")

  comparison <- dplyr::inner_join(
    ard[keys],
    ard[keys],
    by = keys,
    suffix = c(".x", ".y")
  )

  result <- .build_mismatches(comparison, "nonexistent", keys)
  expect_equal(nrow(result), 0L)
  expect_true("nonexistent.x" %in% names(result))
  expect_true("nonexistent.y" %in% names(result))
})
