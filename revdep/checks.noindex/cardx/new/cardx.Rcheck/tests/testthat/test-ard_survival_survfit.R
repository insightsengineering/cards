skip_if_not(is_pkg_installed(c("survival", "broom"), reference_pkg = "cardx"))

test_that("ard_survival_survfit() works with times provided", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survival_survfit(times = c(60, 180)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survival_survfit() works with different type", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survival_survfit(times = c(60, 180), type = "risk") |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survival_survfit() works with probs provided", {
  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survival_survfit(probs = c(0.25, 0.75)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survival_survfit() works with unstratified model", {
  expect_snapshot(
    survival::survfit(survival::Surv(time, status) ~ 1, data = survival::lung) |>
      ard_survival_survfit(times = c(60, 180)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )

  expect_snapshot(
    survival::survfit(survival::Surv(time, status) ~ 1, data = survival::lung) |>
      ard_survival_survfit(probs = c(0.5, 0.75)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survival_survfit() works with multiple stratification variables", {
  expect_snapshot(
    survival::survfit(survival::Surv(time, status) ~ sex + ph.ecog, data = survival::lung) |>
      ard_survival_survfit(times = c(60, 180)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      dplyr::select("group1", "group1_level", "group2", "group2_level") |>
      head(20) |>
      print(n = Inf)
  )

  expect_snapshot(
    survival::survfit(survival::Surv(time, status) ~ sex + ph.ecog, data = survival::lung) |>
      ard_survival_survfit(probs = c(0.5, 0.75)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      dplyr::select("group1", "group1_level", "group2", "group2_level") |>
      head(20) |>
      print(n = Inf)
  )
})

test_that("ard_survival_survfit() works with competing risks", {
  set.seed(1)
  ADTTE_MS <- cards::ADTTE %>%
    dplyr::mutate(
      CNSR = dplyr::case_when(
        CNSR == 0 ~ "censor",
        runif(dplyr::n()) < 0.5 ~ "death from cancer",
        TRUE ~ "death other causes"
      ) %>% factor()
    )

  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, data = ADTTE_MS) %>%
      ard_survival_survfit(times = c(60, 180)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ) |>
      print(n = Inf)
  )
})

test_that("ard_survival_survfit() errors are properly handled", {
  expect_snapshot(
    ard_survival_survfit("not_survfit"),
    error = TRUE
  )

  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survival_survfit(times = 100, type = "notatype"),
    error = TRUE
  )

  expect_snapshot(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survival_survfit(times = 100, probs = c(0.25, 0.75)),
    error = TRUE
  )
})

test_that("ard_survival_survfit() works with non-syntactic names", {
  expect_equal(
    survival::survfit(survival::Surv(time, status) ~ factor(sex) + `ph.ecog`, data = survival::lung) |>
      ard_survival_survfit(times = c(60, 180)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      ),
    survival::survfit(survival::Surv(time, status) ~ sex + ph.ecog, data = survival::lung) |>
      ard_survival_survfit(times = c(60, 180)) |>
      dplyr::mutate(
        stat = lapply(stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x))
      )
  )
})

test_that("ard_survival_survfit() errors with stratified Cox model", {
  withr::local_namespace("survival")
  expect_snapshot(
    error = TRUE,
    coxph(Surv(time, status) ~ age + strata(sex), survival::lung) |>
      survfit() |>
      ard_survival_survfit()
  )
})


test_that("ard_survival_survfit() follows ard structure", {
  expect_silent(
    survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
      ard_survival_survfit(times = c(60, 180)) |>
      cards::check_ard_structure(method = FALSE)
  )
})
