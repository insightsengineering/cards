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
