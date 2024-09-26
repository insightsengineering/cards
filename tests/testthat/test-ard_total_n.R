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

test_that("ard_total_n() follows ard structure", {
  expect_silent(
    ard_total_n(ADSL) |>
      check_ard_structure(method = FALSE)
  )
})
