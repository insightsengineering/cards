skip_if_not(is_pkg_installed("broom"))

test_that("ard_stats_poisson_test() works for one sample tests", {
  # Default values work
  expect_silent(
    ard_stats_poisson_test(cards::ADTTE, variables = c(CNSR, AVAL))
  )

  # Custom values work
  expect_silent(
    ard_single <- ard_stats_poisson_test(
      cards::ADTTE,
      variables = c("CNSR", "AVAL"),
      conf.level = 0.90,
      r = 0.8,
      alternative = "greater"
    )
  )

  # Statistics calculated correctly
  expect_equal(
    ard_single |>
      cards::get_ard_statistics(
        stat_name %in%
          c("estimate", "statistic", "p.value", "parameter", "conf.low", "conf.high", "method", "alternative")
      ),
    poisson.test(
      x = sum(cards::ADTTE$CNSR),
      T = sum(cards::ADTTE$AVAL),
      r = 0.8,
      conf.level = 0.9,
      alternative = "greater"
    ) |>
      broom::tidy() |>
      unclass(),
    ignore_attr = TRUE
  )
})

test_that("ard_stats_poisson_test() works for two sample tests", {
  expect_silent(
    ard_compare <-
      cards::ADTTE |>
      dplyr::filter(TRTA %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_stats_poisson_test(by = TRTA, variables = c(CNSR, AVAL))
  )

  # Statistics calculated correctly
  expect_equal(
    ard_compare |>
      cards::get_ard_statistics(
        stat_name %in%
          c("estimate", "statistic", "p.value", "parameter", "conf.low", "conf.high", "method", "alternative")
      ),
    poisson.test(
      x = cards::ADTTE |>
        dplyr::filter(TRTA %in% c("Placebo", "Xanomeline High Dose")) |>
        dplyr::group_by(TRTA) |>
        dplyr::summarise(sum = sum(CNSR)) |>
        dplyr::pull(sum),
      T = cards::ADTTE |>
        dplyr::filter(TRTA %in% c("Placebo", "Xanomeline High Dose")) |>
        dplyr::group_by(TRTA) |>
        dplyr::summarise(sum = sum(AVAL)) |>
        dplyr::pull(sum)
    ) |>
      broom::tidy() |>
      unclass(),
    ignore_attr = TRUE
  )
})

test_that("ard_stats_poisson_test() errors are handled correctly", {
  expect_snapshot(
    cards::ADTTE |>
      ard_stats_poisson_test(by = TRTA, variables = c(CNSR, AVAL)),
    error = TRUE
  )
})

test_that("ard_stats_poisson_test() follows ard structure", {
  expect_silent(
    ard_stats_poisson_test(cards::ADTTE, variables = c(CNSR, AVAL)) |>
      cards::check_ard_structure(method = T)
  )
})
