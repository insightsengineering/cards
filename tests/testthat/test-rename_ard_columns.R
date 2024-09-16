test_that("rename_ard_columns(columns)", {
  expect_equal(
    ADSL |>
      ard_categorical(by = ARM, variables = AGEGR1) |>
      rename_ard_columns() %>%
      `[`(1:2) |>
      names(),
    c("ARM", "AGEGR1")
  )
})

test_that("rename_ard_columns(unlist)", {
  expect_snapshot(
    ADSL |>
      ard_categorical(by = ARM, variables = AGEGR1) |>
      apply_fmt_fn() |>
      rename_ard_columns(unlist = c(stat, stat_fmt))
  )
})

test_that("rename_ard_columns(unlist) messaging", {
  expect_snapshot(
    ADSL |>
      ard_categorical(by = ARM, variables = AGEGR1) |>
      dplyr::mutate(stat = ifelse(dplyr::row_number() == 1L, list(median), stat)) |>
      rename_ard_columns(unlist = stat) |>
      head(1L)
  )

  expect_snapshot(
    error = TRUE,
    ADSL |>
      ard_categorical(by = ARM, variables = AGEGR1) |>
      dplyr::mutate(stat = ifelse(dplyr::row_number() == 1L, list(NULL), stat)) |>
      rename_ard_columns(unlist = stat)
  )
})
