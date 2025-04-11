skip_if_not(do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom.helpers")))

test_that("ard_regression_basic() works", {
  withr::local_options(list(width = 100))

  expect_error(
    ard <- lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_regression_basic(),
    NA
  )

  expect_snapshot(as.data.frame(ard) |> dplyr::select(-fmt_fn))
})

test_that("ard_regression_basic() does not produce `variable_level` column where not applicable", {
  expect_true(!"variable_level" %in% names(lm(mpg ~ hp, mtcars) |> ard_regression_basic()))
})

test_that("ard_regression_basic() follows ard structure", {
  expect_silent(
    lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_regression_basic() |>
      cards::check_ard_structure(method = FALSE)
  )
})
