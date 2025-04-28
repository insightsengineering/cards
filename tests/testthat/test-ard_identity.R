test_that("ard_identity() works", {
  ttest_result <- t.test(formula = AGE ~ 1, data = ADSL)

  # here we convert a named list to an ARD, then back to a list to ensure accurate conversion
  expect_equal(
    ttest_result[c("statistic", "parameter", "p.value")] |>
      ard_identity(variable = "AGE", context = "ard_onesample_t_test") |>
      get_ard_statistics(),
    ttest_result[c("statistic", "parameter", "p.value")]
  )
})


test_that("ard_identity() messaging", {
  # passing results that are not a named list
  expect_snapshot(
    error = TRUE,
    ard_identity(x = as.list(letters), variable = "AGE")
  )
})
