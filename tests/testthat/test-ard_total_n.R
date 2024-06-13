test_that("ard_total_n() works", {
  expect_snapshot(
    ard_total_n(ADSL) |>
      as.data.frame()
  )

  expect_snapshot(
    error = TRUE,
    ard_total_n(letters)
  )
})
