skip_if_not(is_pkg_installed(pkg = "broom.helpers", reference_pkg = "cardx"))

test_that("ard_regression() works", {
  expect_snapshot(
    lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_regression(add_estimate_to_reference_rows = TRUE) |>
      as.data.frame() |>
      dplyr::select(-context, -stat_label, -fmt_fn) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      )
  )

  # checking non-syntactic names
  expect_equal(
    lm(AGE ~ `Treatment Arm`, data = cards::ADSL |> dplyr::rename(`Treatment Arm` = ARM)) |>
      ard_regression(add_estimate_to_reference_rows = TRUE) |>
      dplyr::pull(variable) |>
      unique(),
    "Treatment Arm"
  )
})

test_that("ard_regression() works specifying custom tidier", {
  skip_if_not(is_pkg_installed(pkg = c("lme4", "broom.mixed"), reference_pkg = "cardx"))
  expect_snapshot(
    lme4::lmer(mpg ~ hp + (1 | cyl), data = mtcars) |>
      ard_regression(tidy_fun = broom.mixed::tidy) |>
      as.data.frame() |>
      dplyr::select(-context, -stat_label, -fmt_fn) |>
      dplyr::filter(map_lgl(stat, is.numeric)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      )
  )
})

test_that("ard_regression() does not produce `variable_level` column where not applicable", {
  expect_true(!"variable_level" %in% names(lm(mpg ~ hp, mtcars) |> ard_regression()))
})
