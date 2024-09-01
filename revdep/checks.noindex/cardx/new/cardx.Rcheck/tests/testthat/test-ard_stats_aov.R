skip_if_not(is_pkg_installed(c("broom.helpers", "parameters"), reference_pkg = "cardx"))

test_that("ard_aov() works", {
  expect_error(
    ard_aov <-
      ard_stats_aov(AGE ~ ARM, data = cards::ADSL),
    NA
  )

  expect_equal(
    ard_aov |>
      cards::get_ard_statistics(stat_name %in% c("sumsq", "statistic")),
    aov(
      AGE ~ ARM,
      data = cards::ADSL
    ) |>
      broom::tidy() |>
      dplyr::slice_head() |>
      dplyr::select(sumsq, statistic) |>
      unclass(),
    ignore_attr = TRUE
  )

  # see if it can handle multiple variables
  expect_snapshot(
    ard_stats_aov(AGE ~ ARM + SEX, data = cards::ADSL) |>
      as.data.frame()
  )
})
