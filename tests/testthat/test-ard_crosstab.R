test_that("ard_crosstab works", {
  expect_error(
    ard_crosstab <- ard_crosstab(ADSL, variables = "AGEGR1", by = "ARM"),
    NA
  )
})
