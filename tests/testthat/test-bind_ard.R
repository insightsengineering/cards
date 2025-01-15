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
    bind_ard(ard, ard, .distinct = FALSE, .update = FALSE),
    error = TRUE
  )
})

test_that("bind_ard() .order argument works", {
  withr::local_options(list(width = 120))
  withr::local_seed(1123)
  expect_snapshot(
    bind_ard(
      ard_categorical(ADSL, by = "ARM", variables = "SEX") %>%
        # randomly sort data
        {dplyr::slice(., sample.int(nrow(.)))}, # styler: off
      .order = TRUE
    ) |>
      as.data.frame() |>
      dplyr::select(-c(context, fmt_fn, warning, error))
  )

  expect_snapshot(
    bind_ard(
      ard_categorical(ADSL, by = "ARM", variables = "SEX") %>%
        # randomly sort data
        {dplyr::slice(., sample.int(nrow(.)))}, # styler: off
      .order = FALSE
    ) |>
      as.data.frame() |>
      dplyr::select(-c(context, fmt_fn, warning, error))
  )
})

test_that("bind_ard(.quiet)", {
  expect_silent(
    ard_continuous(ADSL, variables = AGE) %>%
      {bind_ard(., ., .update = TRUE, .quiet = TRUE)} # styler: off
  )
})

test_that("bind_ard(.distinct)", {
  expect_snapshot(
    ard_continuous(ADSL, variables = AGE) %>%
      {bind_ard(., ., .update = FALSE)} # styler: off
  )
})
