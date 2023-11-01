test_that("ard_crosstab works", {
  expect_error(
    ard_crosstab <- ard_crosstab(ADSL, variables = "AGEGR1", by = "ARM"),
    NA
  )

  expect_equal(
    ard_crosstab(
      mtcars,
      by = am,
      variables = starts_with("xxxxx")
    ),
    dplyr::tibble()
  )
})
