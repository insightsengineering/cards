test_that("rename_ard_columns(columns)", {
  expect_equal(
    ADSL |>
      ard_categorical(by = ARM, variables = AGEGR1) |>
      rename_ard_columns() %>%
      `[`(1:2) |>
      names(),
    c("ARM", "AGEGR1")
  )

  # testing stack output
  expect_silent(
    ard_stack <-
      ard_stack(
        ADSL,
        ard_categorical(variables = AGEGR1),
        .by = ARM
      ) |>
      rename_ard_columns()
  )

  # check the overall ARM tabulations
  expect_equal(
    ard_stack |>
      dplyr::filter(is.na(AGEGR1)) |>
      dplyr::select(-AGEGR1),
    ard_categorical(ADSL, variables = ARM) |>
      rename_ard_columns()
  )
})

test_that("rename_ard_columns(columns) messsaging", {
  expect_snapshot(
    error = TRUE,
    ADSL |>
      ard_categorical(by = ARM, variables = AGEGR1) |>
      rename_ard_columns(columns = all_ard_groups())
  )

  expect_snapshot(
    error = TRUE,
    ADSL |>
      dplyr::rename(stat = AGEGR1) |>
      ard_categorical(by = ARM, variables = stat) |>
      rename_ard_columns()
  )
})

test_that("rename_ard_columns(unlist) lifecycle", {
  lifecycle::expect_deprecated(
    ADSL |>
      ard_categorical(by = ARM, variables = AGEGR1) |>
      rename_ard_columns(unlist = "stat")
  )
})
