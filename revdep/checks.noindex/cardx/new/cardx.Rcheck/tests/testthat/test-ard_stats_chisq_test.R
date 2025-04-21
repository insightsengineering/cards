skip_if_not(is_pkg_installed("broom"))

test_that("ard_stats_chisq_test() works", {
  expect_error(
    ard_chisqtest <-
      cards::ADSL |>
      ard_stats_chisq_test(by = ARM, variables = AGEGR1),
    NA
  )

  expect_equal(
    ard_chisqtest |>
      cards::get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    with(cards::ADSL, chisq.test(AGEGR1, ARM)) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )

  # function works with multiple variables
  expect_equal(
    dplyr::bind_rows(
      ard_chisqtest,
      cards::ADSL |>
        ard_stats_chisq_test(by = ARM, variables = BMIBLGR1)
    ),
    cards::ADSL |>
      ard_stats_chisq_test(by = ARM, variables = c(AGEGR1, BMIBLGR1))
  )
})


test_that("ard_stats_chisq_test() follows ard structure", {
  expect_silent(
    cards::ADSL |>
      ard_stats_chisq_test(by = ARM, variables = AGEGR1) |>
      cards::check_ard_structure()
  )
})
