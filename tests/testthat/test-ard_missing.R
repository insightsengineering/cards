test_that("ard_missing() works", {
  expect_snapshot(
    ard_missing(ADSL, by = "ARM", variables = "AGE") |>
      dplyr::select(-"statistic_fmt_fn") |>
      as.data.frame()
  )
})
