skip_if_not(is_pkg_installed("broom", reference_pkg = "cardx"))

test_that("ard_stats_fisher_test() works", {
  expect_error(
    ard_fishertest <-
      cards::ADSL[1:20, ] |>
      ard_stats_fisher_test(by = ARM, variables = AGEGR1),
    NA
  )

  expect_equal(
    ard_fishertest |>
      cards::get_ard_statistics(stat_name %in% c("p.value", "method")),
    with(cards::ADSL[1:20, ], fisher.test(AGEGR1, ARM)) |>
      broom::tidy() |>
      dplyr::select(p.value, method) |>
      unclass(),
    ignore_attr = TRUE
  )

  # function works with multiple variables
  expect_equal(
    dplyr::bind_rows(
      ard_fishertest,
      cards::ADSL[1:20, ] |>
        ard_stats_fisher_test(by = ARM, variables = BMIBLGR1)
    ),
    cards::ADSL[1:20, ] |>
      ard_stats_fisher_test(by = ARM, variables = c(AGEGR1, BMIBLGR1))
  )
})

test_that("ard_stats_fisher_test() follows ard structure", {
  expect_silent(
    cards::ADSL[1:20, ] |>
      ard_stats_fisher_test(by = ARM, variables = AGEGR1) |>
      cards::check_ard_structure()
  )
})
