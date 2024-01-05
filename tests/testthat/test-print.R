test_that("print.card() works", {
  expect_snapshot(
    ard_continuous(ADSL, by = "ARM", variables = "AGE")
  )

  expect_snapshot(
    ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
  )

  expect_snapshot(
    lm(AGE ~ ARM, data = ADSL) |>
      ard_regression(add_estimate_to_reference_rows = TRUE)
  )
})
