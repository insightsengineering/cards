test_that("print.card() works", {
  expect_snapshot(
    ard_continuous(ADSL, by = "ARM", variables = "AGE")
  )

  expect_snapshot(
    ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
  )

  expect_snapshot(
    ard_continuous(ADSL, variables = "AGE", fmt_fn = AGE ~ list(~ \(x) round(x, 3)))
  )
})
