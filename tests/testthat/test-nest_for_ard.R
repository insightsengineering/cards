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
})
