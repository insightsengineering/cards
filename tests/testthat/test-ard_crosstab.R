test_that("ard_crosstab works", {
  expect_error(
    ard_crosstab <- ard_crosstab(ADSL, variables = "AGEGR1", by = "ARM"),
    NA
  )

  mtrx_conts <- with(ADSL, table(AGEGR1, ARM)) |> unclass()
  mtrx_percs <- mtrx_conts / sum(mtrx_conts)

  expect_equal(
    ard_crosstab |>
      dplyr::filter(group1_level %in% "Placebo", variable_level %in% "<65", stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      unlist(),
    mtrx_conts["<65", "Placebo"]
  )

  expect_equal(
    ard_crosstab |>
      dplyr::filter(group1_level %in% "Placebo", variable_level %in% "<65", stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      unlist(),
    mtrx_percs["<65", "Placebo"]
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
