test_that("ard_strata() works", {
  expect_snapshot(
    ard_strata(
      ADSL,
      strata = ARM,
      ard_continuous,
      variables = AGE
    )
  )

  expect_snapshot(
    ard_strata(
      ADSL,
      strata = ARM,
      ard_continuous,
      variables = AGE,
      by = AGEGR1
    )
  )
})

test_that("ard_strata(strata) messaging", {
  expect_snapshot(
    error = TRUE,
    ard_strata(
      ADSL,
      strata = starts_with("xxxx"),
      ard_continuous,
      variables = AGE
    )
  )
})
