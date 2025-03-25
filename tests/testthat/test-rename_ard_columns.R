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
