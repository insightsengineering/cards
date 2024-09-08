test_that("update_ard_fmt_fn()", {
  expect_equal(
    ard_continuous(ADSL, variables = AGE) |>
      update_ard_fmt_fn(stat_names = c("mean", "sd"), fmt_fn = 8L) |>
      apply_fmt_fn() |>
      dplyr::filter(stat_name %in% c("mean", "sd")) |>
      dplyr::pull("stat_fmt") |>
      unlist(),
    c("75.08661417", "8.24623390")
  )
})

test_that("update_ard_stat_label()", {
  expect_equal(
    ard_continuous(ADSL, variables = AGE) |>
      update_ard_stat_label(stat_names = c("mean", "sd"), stat_label = "Mean (SD)") |>
      apply_fmt_fn() |>
      dplyr::filter(stat_name %in% c("mean", "sd")) |>
      dplyr::pull("stat_label") |>
      unlist() |>
      unique(),
    "Mean (SD)"
  )
})
