test_that("ARD helpers works", {
  ard <-
    ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")

  expect_error(
    get_ard_statistics(
      ard,
      group1_level %in% "Placebo",
      variable_level %in% "65-80"
    ),
    NA
  )

  expect_error(
    bind_ard(ard, ard, .update = TRUE),
    NA
  )
})

test_that("ARD helpers messaging", {
  ard <-
    ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")

  expect_snapshot(
    bind_ard(ard, ard, .update = letters),
    error = TRUE
  )

  expect_snapshot(
    bind_ard(ard, ard, .update = FALSE),
    error = TRUE
  )
})
