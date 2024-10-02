skip_if_not(is_pkg_installed(c("survival", "broom"), reference_pkg = "cardx"))

test_that("ard_survival_survfit_diff() works", {
  withr::local_package("survival")
  sf <- survfit(Surv(AVAL, 1 - CNSR) ~ SEX, cards::ADTTE)
  expect_silent(
    ard1 <- ard_survival_survfit_diff(sf, times = c(25, 50))
  )

  # check the survival differences are accurate
  expect_equal(
    ard1 |>
      dplyr::filter(variable_level == 25, stat_name == "estimate") |>
      dplyr::pull(stat) |>
      unlist(),
    summary(sf, times = 25) |>
      getElement("surv") |>
      reduce(`-`)
  )
  expect_equal(
    ard1 |>
      dplyr::filter(variable_level == 50, stat_name == "estimate") |>
      dplyr::pull(stat) |>
      unlist(),
    summary(sf, times = 50) |>
      getElement("surv") |>
      reduce(`-`)
  )

  # check the structure of the ARD object
  expect_silent(
    cards::check_ard_structure(ard1)
  )
})

test_that("ard_survival_survfit_diff() messaging", {
  withr::local_package("survival")

  # we can only do one stratifying variable at a time
  expect_snapshot(
    error = TRUE,
    survfit(Surv(AVAL, 1 - CNSR) ~ SEX + TRTA, cards::ADTTE) |>
      ard_survival_survfit_diff(times = c(25, 50))
  )

  # the stratifying variable must have 2 or more levels
  expect_snapshot(
    error = TRUE,
    survfit(
      Surv(AVAL, 1 - CNSR) ~ constant,
      cards::ADTTE |> dplyr::mutate(constant = 1L)
    ) |>
      ard_survival_survfit_diff(times = c(25, 50))
  )

  # cannot pass a multi-state model or stratified Cox
  expect_snapshot(
    error = TRUE,
    coxph(Surv(AVAL, CNSR) ~ SEX + strata(TRTA), cards::ADTTE) |>
      survfit() |>
      ard_survival_survfit_diff(times = c(25, 50))
  )
})

test_that("ard_survival_survfit_diff() follows ard structure", {
  withr::local_package("survival")
  sf <- survfit(Surv(AVAL, 1 - CNSR) ~ SEX, cards::ADTTE)
  expect_silent(
    ard_survival_survfit_diff(sf, times = c(25, 50)) |>
      cards::check_ard_structure()
  )
})
