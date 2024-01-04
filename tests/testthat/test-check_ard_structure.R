test_that("check_ard_structure() works", {
  expect_snapshot(
    ard_continuous(ADSL, variables = "AGE") |>
      dplyr::mutate(statistic = unlist(statistic)) |>
      dplyr::select(-error) |>
      structure(class = "data.frame") |>
      check_ard_structure()
  )
})
