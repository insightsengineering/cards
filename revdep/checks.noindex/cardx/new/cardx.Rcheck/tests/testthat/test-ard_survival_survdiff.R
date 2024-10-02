skip_if_not(is_pkg_installed(c("survival", "broom"), reference_pkg = "cardx"))

test_that("ard_survival_survdiff() works", {
  # Log-rank test ----
  expect_error(
    ard_survival_survdiff <-
      ard_survival_survdiff(survival::Surv(AVAL, CNSR) ~ TRTA, data = cards::ADTTE),
    NA
  )

  expect_equal(
    ard_survival_survdiff |>
      dplyr::slice(-nrow(ard_survival_survdiff)) |>
      cards::get_ard_statistics(),
    survival::survdiff(survival::Surv(AVAL, CNSR) ~ TRTA, data = cards::ADTTE) |>
      broom::glance() |>
      as.list()
  )
  expect_equal(
    ard_survival_survdiff$stat[ard_survival_survdiff$stat_name %in% "method"],
    list("Log-rank test")
  )

  # Tarone-Ware test ----
  expect_error(
    ard_survival_survdiff <-
      ard_survival_survdiff(survival::Surv(AVAL, CNSR) ~ TRTA, data = cards::ADTTE, rho = 1.5),
    NA
  )

  expect_equal(
    ard_survival_survdiff |>
      dplyr::slice(-nrow(ard_survival_survdiff)) |>
      cards::get_ard_statistics(),
    survival::survdiff(survival::Surv(AVAL, CNSR) ~ TRTA, data = cards::ADTTE, rho = 1.5) |>
      broom::glance() |>
      as.list()
  )
  expect_equal(
    ard_survival_survdiff$stat[ard_survival_survdiff$stat_name %in% "method"],
    list("Tarone-Ware test")
  )
})

test_that("ard_survival_survdiff() error messaging", {
  expect_error(
    ard_survival_survdiff(survival::Surv(AVAL, CNSR) ~ 1, data = cards::ADTTE),
    "There was an error"
  )

  expect_error(
    ard_survival_survdiff <-
      ard_survival_survdiff(survival::Surv(AVAL, CNSR) ~ not_a_variable, data = cards::ADTTE),
    NA
  )
  expect_true(
    ard_survival_survdiff |> nrow() == 4L
  )
  expect_true(
    ard_survival_survdiff$error |> unique() |> grepl(pattern = "*'not_a_variable'*", x = _)
  )
})

test_that("ard_survival_survdiff() follows ard structure", {
  expect_silent(
    ard_survival_survdiff(survival::Surv(AVAL, CNSR) ~ TRTA, data = cards::ADTTE) |>
      cards::check_ard_structure()
  )
})
