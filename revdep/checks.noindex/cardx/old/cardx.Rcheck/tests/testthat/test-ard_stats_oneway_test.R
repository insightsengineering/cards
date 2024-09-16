skip_if_not(is_pkg_installed("broom", reference_pkg = "cardx"))

test_that("ard_stats_oneway_test() works", {
  expect_error(
    ard_onewaytest <- ard_stats_oneway_test(AGE ~ ARM, data = cards::ADSL),
    NA
  )

  expect_equal(
    ard_onewaytest |>
      cards::get_ard_statistics(stat_name %in% c("num.df", "statistic", "method")),
    oneway.test(
      AGE ~ ARM,
      data = cards::ADSL
    ) |>
      broom::tidy() |>
      dplyr::select(num.df, statistic, method) |>
      unclass(),
    ignore_attr = TRUE
  )

  # warnings are properly handled - "variable" should be continuous, not character
  # THE WARNING HERE IS VERY LONG, SO NOT CONVERTING TO data.frame TO KEEP THE CHECK EASY ON THE EYES
  expect_snapshot(
    ard_stats_oneway_test(AGEGR1 ~ ARM, data = cards::ADSL) |>
      dplyr::select(c("stat_name", "stat", "warning")) |>
      head(3)
  )
})
