test_that("compare_ard identifies stat mismatches", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_summary(dplyr::mutate(ADSL, AGE = AGE + 1), variables = AGE)

  expect_message(
    result <- compare_ard(ard_base, ard_modified),
    "keys"
  )

  expect_s3_class(result, "ard_comparison")
  expect_true("stat" %in% names(result$compare))
  expect_gt(nrow(result$compare$stat), 0L)

  mean_row <- result$compare$stat |>
    dplyr::filter(stat_name == "mean")

  expect_equal(nrow(mean_row), 1L)
  expect_false(identical(mean_row$stat.x[[1]], mean_row$stat.y[[1]]))
})

test_that("compare_ard returns empty data frames when ARDs are identical", {
  ard <- ard_tabulate(ADSL, variables = AGEGR1)

  expect_message(result <- compare_ard(ard, ard))

  expect_s3_class(result, "ard_comparison")
  expect_equal(nrow(result$rows_in_x_not_y), 0L)
  expect_equal(nrow(result$rows_in_y_not_x), 0L)
  expect_true(all(vapply(result$compare, nrow, integer(1)) == 0L))
})

test_that("compare_ard validates duplicates in keys", {
  ard <- ard_tabulate(ADSL, variables = AGEGR1)
  ard_dup <- dplyr::bind_rows(ard, ard)

  expect_error(
    suppressMessages(compare_ard(ard_dup, ard)),
    "Duplicate key combinations"
  )
})

test_that("compare_ard supports custom keys", {
  ard <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_summary(dplyr::mutate(ADSL, AGE = AGE + 1), variables = AGE)

  expect_message(
    result <- compare_ard(ard, ard_modified, keys = c("variable", "stat_name"))
  )

  expect_gt(nrow(result$compare$stat), 0L)
  expect_true(all(c("variable", "stat_name") %in% names(result$compare$stat)))
})

test_that("compare_ard supports custom compare columns", {
  ard <- ard_summary(ADSL, variables = AGE)

  # Modify stat_label
  ard_modified <- ard
  ard_modified$stat_label[1] <- "Modified Label"

  expect_message(
    result <- compare_ard(ard, ard_modified, compare = "stat_label")
  )

  expect_true("stat_label" %in% names(result$compare))
  expect_equal(nrow(result$compare$stat_label), 1L)
})

test_that("compare_ard handles ARDs with different grouping structures", {
  ard_with_group <-
    ard_summary(ADSL, variables = AGE, by = ARM) |>
    dplyr::filter(group1_level == "Placebo")

  ard_without_group <- ard_summary(ADSL, variables = AGE)

  # Should use intersection of keys
  expect_message(
    result <- compare_ard(ard_with_group, ard_without_group)
  )

  expect_s3_class(result, "ard_comparison")
  # Keys should not include group1/group1_level since they're not in both
  expect_false("group1" %in% names(result$compare$stat))
})

test_that("compare_ard detects rows in x not in y", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_subset <- ard_base |> dplyr::filter(stat_name != "mean")

  expect_message(result <- compare_ard(ard_base, ard_subset))

  expect_equal(nrow(result$rows_in_x_not_y), 1L)
  expect_equal(result$rows_in_x_not_y$stat_name, "mean")
  expect_equal(nrow(result$rows_in_y_not_x), 0L)
})

test_that("compare_ard detects rows in y not in x", {
  ard_base <- ard_summary(ADSL, variables = AGE)
  ard_subset <- ard_base |> dplyr::filter(stat_name != "mean")

  expect_message(result <- compare_ard(ard_subset, ard_base))

  expect_equal(nrow(result$rows_in_x_not_y), 0L)
  expect_equal(nrow(result$rows_in_y_not_x), 1L)
  expect_equal(result$rows_in_y_not_x$stat_name, "mean")
})

test_that("compare_ard errors when keys are empty", {
  ard1 <- ard_summary(ADSL, variables = AGE) |>
    dplyr::select(-variable, -stat_name)
  ard2 <- ard_summary(ADSL, variables = BMIBL) |>
    dplyr::select(-variable, -stat_name)

  # Error comes from cards_select, not our check
  expect_error(
    compare_ard(ard1, ard2, keys = "nonexistent_col")
  )
})

test_that("compare_ard errors when compare columns are empty", {
  ard <- ard_summary(ADSL, variables = AGE)

  # Error comes from cards_select, not our check
  expect_error(
    suppressMessages(compare_ard(ard, ard, compare = "nonexistent_col"))
  )
})

test_that("compare_ard handles missing columns gracefully", {
  ard <- ard_summary(ADSL, variables = AGE)
  ard_no_stat_fmt <- ard |> dplyr::select(-any_of("stat_fmt"))

  # stat_fmt not in either, but stat and stat_label are
  expect_message(
    result <- compare_ard(ard_no_stat_fmt, ard_no_stat_fmt, compare = any_of(c("stat", "stat_label")))
  )

  expect_true(all(vapply(result$compare, nrow, integer(1)) == 0L))
})

test_that("compare_ard works with by variables", {
  ard1 <- ard_tabulate(ADSL, by = ARM, variables = AGEGR1)
  ard2 <- ard1
  # Modify one value
  ard2$stat[1] <- list(999L)

  expect_message(result <- compare_ard(ard1, ard2))

  expect_equal(nrow(result$compare$stat), 1L)
  expect_true("group1" %in% names(result$compare$stat))
  expect_true("group1_level" %in% names(result$compare$stat))
})

test_that("compare_ard validates input classes", {
  expect_error(compare_ard(data.frame(), ard_summary(ADSL, variables = AGE)), class = "check_class")
  expect_error(compare_ard(ard_summary(ADSL, variables = AGE), data.frame()), class = "check_class")
})

test_that("compare_ard prints informative messages about keys and compare columns", {
  ard <- ard_summary(ADSL, variables = AGE)

  expect_message(
    compare_ard(ard, ard),
    regexp = "keys"
  )

  expect_message(
    compare_ard(ard, ard),
    regexp = "compare"
  )
})

test_that("compare_ard returns ard_comparison class", {
  ard <- ard_summary(ADSL, variables = AGE)

  expect_message(result <- compare_ard(ard, ard))

  expect_s3_class(result, "ard_comparison")
  expect_true("rows_in_x_not_y" %in% names(result))
  expect_true("rows_in_y_not_x" %in% names(result))
  expect_true("compare" %in% names(result))
  expect_true(is.list(result$compare))
})
