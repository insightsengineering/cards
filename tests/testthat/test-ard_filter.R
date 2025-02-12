skip_on_cran()

ADAE_subset <- cards::ADAE |>
  dplyr::filter(AETERM %in% unique(cards::ADAE$AETERM)[1:5])

ard <- ard_stack_hierarchical(
  data = ADAE_subset,
  variables = c(SEX, RACE, AETERM),
  by = TRTA,
  denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
  id = USUBJID,
  over_variables = TRUE
)

test_that("ard_filter() works", {
  withr::local_options(width = 200)

  expect_silent(ard_f <- ard_filter(ard, n > 10))
  expect_snapshot(ard_f)
  expect_equal(nrow(ard_f), 84)

  expect_silent(ard_f <- ard_filter(ard, p > 0.05))
  expect_equal(nrow(ard_f), 171)
})

test_that("ard_filter() works with non-standard filters", {
  expect_silent(ard_f <- ard_filter(ard, n == 2 & p < 0.05))
  expect_equal(nrow(ard_f), 90)

  expect_silent(ard_f <- ard_filter(ard, sum(n) > 4))
  expect_equal(nrow(ard_f), 162)

  expect_silent(ard_f <- ard_filter(ard, mean(n) > 4 | n > 3))
  expect_equal(nrow(ard_f), 135)

  expect_silent(ard_f <- ard_filter(ard, any(n > 5 & TRTA == "Xanomeline High Dose")))
  expect_equal(nrow(ard_f), 117)
})

test_that("ard_filter() returns only summary rows when all rows filtered out", {
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    include = "AETERM",
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID
  )

  expect_silent(ard_f <- ard_filter(ard, n > 200))
  expect_equal(
    ard_f,
    ard |> dplyr::filter(variable != "AETERM")
  )

  expect_true(all(ard_f$variable == "TRTA"))
})

test_that("ard_filter() works with only one variable in x", {
  ard_single <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = AETERM,
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID
  )

  expect_silent(ard_single <- ard_filter(ard_single, n > 20))
  expect_equal(nrow(ard_single), 15)
})

test_that("ard_filter() works when some variables not included in x", {
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    include = c(SEX, AETERM),
    over_variables = TRUE
  )

  expect_silent(ard_filter(ard, n > 10))
})

test_that("ard_filter() error messaging works", {
  # invalid x input
  expect_snapshot(
    ard_filter(ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"), n > 10),
    error = TRUE
  )

  # invalid filter input
  expect_snapshot(
    ard_filter(ard, 10),
    error = TRUE
  )

  # invalid filter parameters
  expect_snapshot(
    ard_filter(ard, A > 5),
    error = TRUE
  )
})
