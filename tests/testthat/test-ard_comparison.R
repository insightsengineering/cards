test_that("ard_ttest() works", {
  expect_error(
    ard_ttest <-
      ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_ttest(by = ARM, variable = AGE, var.equal = TRUE),
    NA
  )

  expect_equal(
    ard_ttest |>
      get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    t.test(
      AGE ~ ARM,
      data = ADSL |> dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")),
      var.equal = TRUE
    ) |>
      broom::tidy() |>
      dplyr::select(estimate, conf.low, conf.high) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_snapshot(
    ADSL |>
      ard_ttest(by = ARM, variable = AGE, var.equal = TRUE) |>
      as.data.frame()
  )
})

test_that("ard_wilcoxtest() works", {
  expect_error(
    ard_wilcoxtest <-
      ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_wilcoxtest(by = ARM, variable = AGE),
    NA
  )

  expect_equal(
    ard_wilcoxtest |>
      get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    wilcox.test(
      AGE ~ ARM,
      data = ADSL |> dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose"))
    ) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )
})

test_that("ard_chisqtest() works", {
  expect_error(
    ard_chisqtest <-
      ADSL |>
      ard_chisqtest(by = ARM, variable = AGEGR1),
    NA
  )

  expect_equal(
    ard_chisqtest |>
      get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    with(ADSL, chisq.test(AGEGR1, ARM)) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )
})

test_that("ard_fishertest() works", {
  expect_error(
    ard_fishertest <-
      ADSL[1:20, ] |>
      ard_fishertest(by = ARM, variable = AGEGR1),
    NA
  )

  expect_equal(
    ard_fishertest |>
      get_ard_statistics(stat_name %in% c("p.value", "method")),
    with(ADSL[1:20, ], fisher.test(AGEGR1, ARM)) |>
      broom::tidy() |>
      dplyr::select(p.value, method) |>
      unclass(),
    ignore_attr = TRUE
  )
})
