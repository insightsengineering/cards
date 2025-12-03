test_that("ard_compare falls back to shared primary keys", {
  ard_with_group <-
    ard_summary(ADSL, variables = AGE, by = ARM) |>
    dplyr::filter(group1_level == "Placebo")

  ard_without_group <- ard_summary(ADSL, variables = AGE)

  result <- ard_compare(ard_with_group, ard_without_group)
  expect_identical(names(result), c("stat", "fmt_fn", "warning", "error"))

  stat_result <- result$stat

  placebo_mean <-
    ard_with_group |>
    dplyr::filter(stat_name == "mean") |>
    dplyr::pull(stat)
  placebo_mean <- placebo_mean[[1]]

  overall_mean <-
    ard_without_group |>
    dplyr::filter(stat_name == "mean") |>
    dplyr::pull(stat)
  overall_mean <- overall_mean[[1]]

  mean_row <- stat_result |>
    dplyr::filter(stat_name == "mean")

  expect_equal(mean_row$stat.x[[1]], placebo_mean)
  expect_equal(mean_row$stat.y[[1]], overall_mean)
  expect_false(any(names(stat_result) %in% c("group1", "group1_level")))
})

test_that("ard_compare errors when shared primary keys are not unique", {
  ard_with_groups <- ard_summary(ADSL, variables = AGE, by = ARM)
  ard_without_groups <- ard_summary(ADSL, variables = AGE)

  err <- expect_error(
    ard_compare(ard_with_groups, ard_without_groups),
    "do not uniquely identify rows"
  )
  expect_match(err$message, "Columns used")
  expect_match(err$message, "variable = \"AGE\"")
  expect_match(err$message, "stat_name = \"mean\"")
})

test_that("ard_compare validates duplicates", {
  ard <- ard_tabulate(ADSL, variables = AGEGR1)
  ard_dup <- dplyr::bind_rows(ard, ard)

  err <- expect_error(ard_compare(ard_dup, ard), "Duplicate key combinations")
  expect_match(err$message, "Columns used")
  expect_match(err$message, "variable = \"AGEGR1\"")
  expect_match(err$message, "stat_name = \"n\"")
})

test_that("ard_compare identifies mismatches", {
  ard_placebo <-
    ard_tabulate(ADSL, by = ARM, variables = AGEGR1) |>
    dplyr::filter(group1_level == "Placebo")
  ard_low_dose <-
    ard_tabulate(ADSL, by = ARM, variables = AGEGR1) |>
    dplyr::filter(group1_level == "Xanomeline Low Dose")

  result <- ard_compare(ard_placebo, ard_low_dose)

  expect_gt(nrow(result$stat), 0L)
  expect_true(any(!mapply(identical, result$stat$stat.x, result$stat$stat.y)))
})

test_that("ard_compare separates fmt_fun, warning, and error mismatches", {
  base_ard <- ard_summary(ADSL, variables = AGE)

  fmt_index <- which(base_ard$stat_name == "mean")[1]
  warning_index <- which(base_ard$stat_name == "sd")[1]
  error_index <- which(base_ard$stat_name == "min")[1]

  original_fmt <- base_ard$fmt_fun[[fmt_index]]
  original_warning <- base_ard$warning[[warning_index]]
  original_error <- base_ard$error[[error_index]]

  modified_ard <- base_ard
  new_fmt <- \(x) "modified"
  modified_ard$fmt_fun[[fmt_index]] <- new_fmt
  modified_ard$warning[[warning_index]] <- "Check warning"
  modified_ard$error[[error_index]] <- "Check error"

  result <- ard_compare(base_ard, modified_ard)

  expect_equal(nrow(result$stat), 0L)
  expect_equal(nrow(result$fmt_fn), 1L)
  expect_equal(nrow(result$warning), 1L)
  expect_equal(nrow(result$error), 1L)

  expect_equal(result$fmt_fn$stat_name, base_ard$stat_name[fmt_index])
  expect_identical(result$fmt_fn$fmt_fun.x[[1]], original_fmt)
  expect_identical(result$fmt_fn$fmt_fun.y[[1]], new_fmt)

  expect_equal(result$warning$stat_name, base_ard$stat_name[warning_index])
  expect_identical(result$warning$warning.x[[1]], original_warning)
  expect_identical(result$warning$warning.y[[1]], "Check warning")

  expect_equal(result$error$stat_name, base_ard$stat_name[error_index])
  expect_identical(result$error$error.x[[1]], original_error)
  expect_identical(result$error$error.y[[1]], "Check error")
})

test_that("ard_compare returns no rows when equal", {
  ard <- ard_tabulate(ADSL, variables = AGEGR1)

  result <- ard_compare(ard, ard)
  expect_true(all(vapply(result, nrow, integer(1)) == 0L))
})

test_that("ard_compare supports custom key columns", {
  ard <- ard_summary(ADSL, variables = AGE)
  ard_modified <- ard_summary(dplyr::mutate(ADSL, AGE = AGE + 1), variables = AGE)

  result <- ard_compare(ard, ard_modified, key_columns = c("stat_name"))

  expect_gt(nrow(result$stat), 0L)
  mean_row <- result$stat |>
    dplyr::filter(stat_name == "mean")
  ard_mean_stat <-
    ard |>
    dplyr::filter(stat_name == "mean") |>
    dplyr::pull(stat)
  ard_mean_stat <- ard_mean_stat[[1]]

  ard_modified_mean_stat <-
    ard_modified |>
    dplyr::filter(stat_name == "mean") |>
    dplyr::pull(stat)
  ard_modified_mean_stat <- ard_modified_mean_stat[[1]]

  expect_equal(mean_row$stat.x[[1]], ard_mean_stat)
  expect_equal(mean_row$stat.y[[1]], ard_modified_mean_stat)
})

test_that("ard_compare accepts tidyselect key columns", {
  ard <- ard_tabulate(ADSL, by = ARM, variables = AGEGR1)
  arm_levels <- if (is.factor(ADSL$ARM)) levels(ADSL$ARM) else unique(ADSL$ARM)
  ard_modified <- ard_tabulate(
    ADSL |> dplyr::mutate(ARM = factor(ARM, levels = rev(arm_levels))),
    by = ARM,
    variables = AGEGR1
  )

  result <- ard_compare(
    ard,
    ard_modified,
    key_columns = c(group1, group1_level, variable_level, stat_name)
  )

  expect_gt(nrow(result$stat), 0L)
  expect_true(all(c("group1_level", "variable_level") %in% names(result$stat)))
  placebo_rows <- result$stat |>
    dplyr::filter(group1_level == "Placebo", stat_name == "n")
  expect_true(nrow(placebo_rows) > 0)
})

test_that("ard_compare validates creation environment metadata", {
  base_ard <- ard_summary(ADSL, variables = AGE)

  shared_env <- new.env(parent = emptyenv())
  ard_x <- base_ard
  attr(ard_x, "cards_env") <- shared_env

  ard_y <- base_ard
  attr(ard_y, "cards_env") <- shared_env

  expect_error(ard_compare(ard_x, ard_y), NA)

  attr(ard_y, "cards_env") <- new.env(parent = emptyenv())

  err <- expect_error(ard_compare(ard_x, ard_y), "environment")
  expect_match(err$message, "cards_env environment")

  attr(ard_x, "cards_env") <- NULL
  expect_error(ard_compare(ard_x, ard_y), "does not store its creation environment")
})
