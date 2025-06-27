test_that("update_ard_fmt_fun()", {
  expect_equal(
    ard_continuous(ADSL, variables = AGE) |>
      update_ard_fmt_fun(stat_names = c("mean", "sd"), fmt_fun = 8L) |>
      apply_fmt_fun() |>
      dplyr::filter(stat_name %in% c("mean", "sd")) |>
      dplyr::pull("stat_fmt") |>
      unlist(),
    c("75.08661417", "8.24623390")
  )

  expect_snapshot(
    error = TRUE,
    ard_continuous(ADSL, variables = AGE) |>
      update_ard_fmt_fun(stat_names = c("mean", "sd"), fmt_fun = -8L)
  )
})

test_that("update_ard_fmt_fun(filter)", {
  # apply update to the Placebo level
  expect_snapshot(
    ard_continuous(ADSL, by = ARM, variables = AGE, statistic = ~ continuous_summary_fns(c("N", "mean"))) |>
      update_ard_fmt_fun(stat_names = "mean", fmt_fun = 8L, filter = group1_level == "Placebo") |>
      apply_fmt_fun()
  )
})

test_that("update_ard_fmt_fun(filter) messaging", {
  # test error messaging
  expect_snapshot(
    error = TRUE,
    ard_continuous(ADSL, by = ARM, variables = AGE, statistic = ~ continuous_summary_fns(c("N", "mean"))) |>
      update_ard_fmt_fun(stat_names = "mean", fmt_fun = 8L, filter = group99999999_level == "Placebo")
  )

  expect_snapshot(
    error = TRUE,
    ard_continuous(ADSL, by = ARM, variables = AGE, statistic = ~ continuous_summary_fns(c("N", "mean"))) |>
      update_ard_fmt_fun(stat_names = "mean", fmt_fun = 8L, filter = c(TRUE, FALSE))
  )
})

test_that("update_ard_stat_label()", {
  expect_equal(
    ard_continuous(ADSL, variables = AGE) |>
      update_ard_stat_label(stat_names = c("mean", "sd"), stat_label = "Mean (SD)") |>
      apply_fmt_fun() |>
      dplyr::filter(stat_name %in% c("mean", "sd")) |>
      dplyr::pull("stat_label") |>
      unlist() |>
      unique(),
    "Mean (SD)"
  )
})

test_that("update_ard_stat_label(filter)", {
  # apply update to the Placebo level
  expect_snapshot(
    ard_continuous(ADSL, by = ARM, variables = AGE, statistic = ~ continuous_summary_fns(c("N", "mean", "sd"))) |>
      update_ard_stat_label(stat_names = c("mean", "sd"), stat_label = "Mean (SD)", filter = group1_level == "Placebo")
  )
})

test_that("update_ard_stat_label(filter) messaging", {
  # test error messaging
  expect_snapshot(
    error = TRUE,
    ard_continuous(ADSL, by = ARM, variables = AGE, statistic = ~ continuous_summary_fns(c("N", "mean", "sd"))) |>
      update_ard_stat_label(stat_names = c("mean", "sd"), stat_label = "Mean (SD)", filter = group99999999_level == "Placebo")
  )

  expect_snapshot(
    error = TRUE,
    ard_continuous(ADSL, by = ARM, variables = AGE, statistic = ~ continuous_summary_fns(c("N", "mean", "sd"))) |>
      update_ard_stat_label(stat_names = c("mean", "sd"), stat_label = "Mean (SD)", filter = c(TRUE, FALSE))
  )
})
