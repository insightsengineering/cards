test_that("ard_continuous_ci.data.frame(method = 't.test')", {
  skip_if_not(is_pkg_installed("broom", reference_pkg = "cardx"))

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
  skip_if_not(is_pkg_installed("broom", reference_pkg = "cardx"))
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

test_that("ard_continuous_ci.data.frame() follows ard structure", {
  skip_if_not(is_pkg_installed("broom", reference_pkg = "cardx"))

  expect_silent(
    ard_continuous_ci(
      mtcars,
      variables = mpg,
      method = "wilcox.test"
    ) |>
      cards::check_ard_structure()
  )
})
