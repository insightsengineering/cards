skip_if_not(do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom.helpers", reference_pkg = "cardx")))

test_that("ard_regression_basic() works", {
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
