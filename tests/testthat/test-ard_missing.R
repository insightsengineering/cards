test_that("ard_missing() works", {
  expect_error(
    ard <- ard_missing(ADSL, by = "ARM", variables = "BMIBL"),
    NA
  )

  expect_snapshot(
    ard |>
      dplyr::select(-"statistic_fmt_fn") |>
      as.data.frame()
  )

  # confirm missing rate is correct
  expect_equal(
    ard |>
      dplyr::filter(stat_name %in% "p_miss") |>
      dplyr::pull(statistic) |>
      unlist(),
    ADSL |>
      dplyr::mutate(BMIBL = is.na(BMIBL)) |>
      dplyr::summarise(
        .by = ARM,
        statistic = mean(BMIBL)
      ) |>
      dplyr::pull(statistic)
  )
})
