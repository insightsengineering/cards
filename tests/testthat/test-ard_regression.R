test_that("ard_regression() works", {
  expect_snapshot(
    lm(AGE ~ ARM, data = ADSL) |>
      ard_regression(add_estimate_to_reference_rows = TRUE) |>
      flatten_ard() |>
      as.data.frame()
  )
})
