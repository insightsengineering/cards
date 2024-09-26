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

test_that("ard_total_n() errors with incomplete factor columns", {
  # First check output is fine when there is a valid factor variable
  expect_snapshot(
    mtcars |>
      dplyr::mutate(am = factor(am)) |>
      ard_total_n()
  )

  # Check error when factors have no levels
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = character(0))) |>
      ard_total_n()
  )

  # Check error when factor has NA level
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = c(0, 1, NA), exclude = NULL)) |>
      ard_total_n()
  )
})
