# Tests that exercise internal helpers in R/compare_ard_helpers.R
# via the exported compare_ard(), is_ard_equal(), and check_ard_equal().

# --- .process_keys_arg() / .check_not_empty() --------------------------------

test_that("compare_ard() works with custom keys", {
  ard <- ard_summary(ADSL, variables = AGE)

  result <- compare_ard(ard, ard, keys = c("variable", "stat_name"))
  expect_equal(result$keys, c("variable", "stat_name"))
  expect_true(is_ard_equal(result))
})

test_that("compare_ard() uses default keys when not specified", {
  ard <- ard_summary(ADSL, variables = AGE)

  result <- compare_ard(ard, ard)
  expect_true("stat_name" %in% result$keys)
  expect_true("variable" %in% result$keys)
})

# --- .process_compare_arg() ---------------------------------------------------

test_that("compare_ard() works with custom columns", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_base
  ard_modified$stat_label[1] <- "Changed"

  result <- compare_ard(ard_base, ard_modified, columns = "stat_label")
  expect_equal(result$columns, "stat_label")
  expect_gt(nrow(result$comparison$stat_label), 0L)
})

# --- .check_keys_unique() / .format_duplicate_keys() / .format_key_value() ----

test_that("compare_ard() errors when x has duplicate keys", {
  ard <- ard_tabulate(ADSL, variables = AGEGR1)
  ard_dup <- dplyr::bind_rows(ard, ard)

  expect_error(
    suppressMessages(compare_ard(ard_dup, ard)),
    "Duplicate key combinations"
  )
})

test_that("compare_ard() errors when y has duplicate keys", {
  ard <- ard_tabulate(ADSL, variables = AGEGR1)
  ard_dup <- dplyr::bind_rows(ard, ard)

  expect_error(
    suppressMessages(compare_ard(ard, ard_dup)),
    "Duplicate key combinations"
  )
})

test_that("compare_ard() duplicate key error includes formatted key values", {
  ard <- ard_tabulate(ADSL, variables = AGEGR1)
  ard_dup <- dplyr::bind_rows(ard, ard)

  # the error message should contain formatted key=value pairs
  expect_error(
    suppressMessages(compare_ard(ard, ard_dup)),
    "variable"
  )
})

# --- .compare_rows() ----------------------------------------------------------

test_that("compare_ard() detects rows in x not in y", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_subset <- ard_base |> dplyr::filter(stat_name != "mean")

  result <- compare_ard(ard_base, ard_subset)
  expect_gt(nrow(result$rows_in_x_not_y), 0L)
  expect_true("mean" %in% result$rows_in_x_not_y$stat_name)
})

test_that("compare_ard() detects rows in y not in x", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_subset <- ard_base |> dplyr::filter(stat_name != "mean")

  result <- compare_ard(ard_subset, ard_base)
  expect_gt(nrow(result$rows_in_y_not_x), 0L)
  expect_true("mean" %in% result$rows_in_y_not_x$stat_name)
})

test_that("compare_ard() returns empty row diffs for identical ARDs", {
  ard <- ard_summary(ADSL, variables = AGE)

  result <- compare_ard(ard, ard)
  expect_equal(nrow(result$rows_in_x_not_y), 0L)
  expect_equal(nrow(result$rows_in_y_not_x), 0L)
})

test_that("compare_ard() detects rows missing from both sides", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_other <- ard_summary(ADSL, variables = BMIBL)

  result <- compare_ard(ard_base, ard_other)
  expect_gt(nrow(result$rows_in_x_not_y), 0L)
  expect_gt(nrow(result$rows_in_y_not_x), 0L)
})

# --- .compare_columns() ------------------------------------------------------

test_that("compare_ard() detects stat value differences", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_summary(dplyr::mutate(ADSL, AGE = AGE + 1), variables = AGE)

  result <- compare_ard(ard_base, ard_modified)
  expect_gt(nrow(result$comparison$stat), 0L)
  expect_true("difference" %in% names(result$comparison$stat))
})

test_that("compare_ard() returns empty comparison for identical ARDs", {
  ard <- ard_summary(ADSL, variables = AGE)

  result <- compare_ard(ard, ard)
  expect_equal(nrow(result$comparison$stat), 0L)
})

test_that("compare_ard() difference column contains all.equal descriptions", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_summary(dplyr::mutate(ADSL, AGE = AGE + 1), variables = AGE)

  result <- compare_ard(ard_base, ard_modified)
  diffs <- result$comparison$stat

  expect_type(diffs$difference, "list")
  expect_true(all(vapply(diffs$difference, is.character, logical(1))))
})

test_that("compare_ard(tolerance) controls numeric comparison sensitivity", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_base
  mean_idx <- which(ard_modified$stat_name == "mean")
  ard_modified$stat[[mean_idx]] <- ard_modified$stat[[mean_idx]] + 1e-10

  # default tolerance ignores tiny diff
  result_default <- compare_ard(ard_base, ard_modified)
  expect_equal(
    nrow(dplyr::filter(result_default$comparison$stat, stat_name == "mean")),
    0L
  )

  # strict tolerance catches it
  result_strict <- compare_ard(ard_base, ard_modified, tolerance = 0)
  expect_equal(
    nrow(dplyr::filter(result_strict$comparison$stat, stat_name == "mean")),
    1L
  )
})

test_that("compare_ard(check.attributes) controls attribute comparison", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_base
  mean_idx <- which(ard_modified$stat_name == "mean")
  val <- ard_modified$stat[[mean_idx]]
  ard_modified$stat[[mean_idx]] <- stats::setNames(val, "mean_val")

  # with check.attributes = TRUE, attribute difference is detected
  result_attrs <- compare_ard(ard_base, ard_modified, check.attributes = TRUE)
  expect_equal(
    nrow(dplyr::filter(result_attrs$comparison$stat, stat_name == "mean")),
    1L
  )

  # with check.attributes = FALSE, attribute difference is ignored
  result_no_attrs <- compare_ard(ard_base, ard_modified, check.attributes = FALSE)
  expect_equal(
    nrow(dplyr::filter(result_no_attrs$comparison$stat, stat_name == "mean")),
    0L
  )
})

test_that("compare_ard() compares multiple columns simultaneously", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_base
  ard_modified$stat[1] <- list(999L)
  ard_modified$stat_label[1] <- "Changed"

  result <- compare_ard(ard_base, ard_modified, columns = any_of(c("stat", "stat_label")))
  expect_true("stat" %in% names(result$comparison))
  expect_true("stat_label" %in% names(result$comparison))
  expect_gt(nrow(result$comparison$stat), 0L)
  expect_gt(nrow(result$comparison$stat_label), 0L)
})
