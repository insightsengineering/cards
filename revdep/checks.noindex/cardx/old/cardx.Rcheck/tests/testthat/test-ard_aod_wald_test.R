skip_if_not(do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "aod", reference_pkg = "cardx")))

test_that("ard_aod_wald_test() works", {
  # works for a generic case
  expect_error(
    glm_ard_aod_waldtest <-
      suppressWarnings(lm(AGE ~ ARM, data = cards::ADSL)) |>
      ard_aod_wald_test(),
    NA
  )
  expect_equal(nrow(glm_ard_aod_waldtest), 6L)
  expect_snapshot(glm_ard_aod_waldtest[, 1:6])

  # error returned when a regression model isn't passed

  expect_error(
    ard_aod_wald_test(cards::ADSL) |>
      dplyr::select(c(context, error))
  )
})
