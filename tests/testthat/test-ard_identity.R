test_that("ard_identity() works", {
  ttest_result <- t.test(formula = AGE ~ 1, data = ADSL)
  lst_result <- ttest_result[c("statistic", "parameter", "p.value")]

  # here we convert a named list to an ARD, then back to a list to ensure accurate conversion
  expect_equal(
    ard_identity(lst_result, variable = "AGE", context = "ard_onesample_t_test") |>
      get_ard_statistics(),
    lst_result[c("statistic", "parameter", "p.value")]
  )
  expect_equal(
    as.data.frame(lst_result) |>
      ard_identity(variable = "AGE", context = "ard_onesample_t_test") |>
      get_ard_statistics(),
    lst_result[c("statistic", "parameter", "p.value")],
    ignore_attr = TRUE
  )

  expect_silent(
    as.data.frame(lst_result) %>%
      {dplyr::bind_rows(., ., .)} |> #styler: off
      ard_identity(variable = "AGE", context = "ard_onesample_t_test") |>
      get_ard_statistics()
  )
})


test_that("ard_identity() messaging", {
  # passing results that are not a named list
  expect_snapshot(
    error = TRUE,
    ard_identity(x = as.list(letters), variable = "AGE")
  )
})
