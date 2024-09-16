test_that("ard_continuous_ci.data.frame(method = 't.test')", {
  expect_equal(
    ard_continuous_ci(
      mtcars,
      variables = mpg,
      method = "t.test"
    ) |>
      dplyr::select(-context),
    ard_stats_t_test_onesample(
      mtcars,
      variables = mpg
    ) |>
      dplyr::select(-context)
  )
})

test_that("ard_continuous_ci.data.frame(method = 'wilcox.test')", {
  expect_equal(
    ard_continuous_ci(
      mtcars,
      variables = mpg,
      method = "wilcox.test"
    ) |>
      dplyr::select(-context),
    ard_stats_wilcox_test_onesample(
      mtcars,
      variables = mpg,
      conf.int = TRUE
    ) |>
      dplyr::select(-context)
  )
})
