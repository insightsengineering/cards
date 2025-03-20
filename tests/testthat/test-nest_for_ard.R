test_that("nest_for_ard() works", {
  expect_equal(
    nest_for_ard(mtcars, strata = c("cyl", "gear"), rename = TRUE) |>
      nrow(),
    8L
  )

  expect_equal(
    nest_for_ard(mtcars, rename = TRUE) |>
      nrow(),
    1L
  )

  expect_equal(
    nest_for_ard(mtcars, by = "am", strata = c("cyl", "gear"), rename = TRUE) |>
      nrow(),
    16L
  )

  # check order of lgl variables (see Issue #411)
  expect_equal(
    mtcars |>
      dplyr::mutate(am = as.logical(am)) |>
      nest_for_ard(by = "am", include_data = FALSE) |>
      dplyr::pull(group1_level) |>
      unlist(),
    c(FALSE, TRUE)
  )
})
