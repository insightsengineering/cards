skip_if_not(is_pkg_installed("broom"))

test_that("ard_stats_kruskal_test() works", {
  expect_error(
    ard_kruskaltest <-
      cards::ADSL |>
      ard_stats_kruskal_test(by = ARM, variables = AGE),
    NA
  )

  expect_equal(
    ard_kruskaltest |>
      cards::get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    with(cards::ADSL, kruskal.test(AGE, ARM)) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_snapshot(
    cards::ADSL |>
      ard_stats_kruskal_test(by = "ARM", variables = "AGE") |>
      as.data.frame()
  )

  # test that the function works with multiple variables
  expect_equal(
    dplyr::bind_rows(
      ard_kruskaltest,
      cards::ADSL |>
        ard_stats_kruskal_test(by = ARM, variable = BMIBL)
    ),
    cards::ADSL |>
      ard_stats_kruskal_test(by = ARM, variable = c(AGE, BMIBL))
  )
})

test_that("ard_stats_kruskal_test() follows ard structure", {
  expect_silent(
    cards::ADSL |>
      ard_stats_kruskal_test(by = ARM, variable = BMIBL) |>
      cards::check_ard_structure()
  )
})
