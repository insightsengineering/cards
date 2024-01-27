test_that("bind_ard() works", {
  ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")

  expect_error(
    bind_ard(ard, ard, .update = TRUE),
    NA
  )
})


test_that("ARD helpers messaging", {
  ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")

  expect_snapshot(
    bind_ard(ard, ard, .update = letters),
    error = TRUE
  )

  expect_snapshot(
    bind_ard(ard, ard, .update = FALSE),
    error = TRUE
  )
})

test_that("bind_ard() .order argument works", {
  expect_snapshot(
    bind_ard(
      ard_categorical(ADSL, by = "ARM", variables = c("SEX", "AGEGR1")),
      ard_chisqtest(ADSL, by = "ARM", variable = "AGEGR1"),
      ard_chisqtest(ADSL, by = "ARM", variable = "SEX"),
      .order = TRUE
    ) |>
      as.data.frame() |>
      dplyr::select(-c(context, statistic_fmt_fn, warning, error))
  )

  expect_snapshot(
    bind_ard(
      ard_categorical(ADSL, by = "ARM", variables = c("SEX", "AGEGR1")),
      ard_chisqtest(ADSL, by = "ARM", variable = "AGEGR1"),
      ard_chisqtest(ADSL, by = "ARM", variable = "SEX"),
      .order = FALSE
    ) |>
      as.data.frame() |>
      dplyr::select(-c(context, statistic_fmt_fn, warning, error))
  )
})
