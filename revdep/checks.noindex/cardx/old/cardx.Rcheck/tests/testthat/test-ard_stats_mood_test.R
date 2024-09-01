skip_if_not(is_pkg_installed("broom", reference_pkg = "cardx"))

test_that("ard_stats_mood_test() works", {
  expect_error(
    ard_moodtest <-
      cards::ADSL |>
      ard_stats_mood_test(by = SEX, variable = AGE),
    NA
  )

  expect_equal(
    ard_moodtest |>
      cards::get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    with(cards::ADSL, mood.test(AGE ~ SEX)) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_snapshot(
    cards::ADSL |>
      ard_stats_mood_test(by = SEX, variable = AGE) |>
      as.data.frame()
  )

  expect_equal(
    dplyr::bind_rows(
      ard_moodtest,
      cards::ADSL |>
        ard_stats_mood_test(by = SEX, variable = BMIBL)
    ),
    cards::ADSL |>
      ard_stats_mood_test(by = SEX, variable = c(AGE, BMIBL))
  )
})
