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

test_that("filter_ard_hierarchical() works", {
  withr::local_options(width = 200)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, n > 10))
  expect_snapshot(ard_f)
  expect_equal(nrow(ard_f), 39)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, p > 0.05))
  expect_equal(nrow(ard_f), 171)
})

test_that("filter_ard_hierarchical() works with non-standard filters", {
  expect_silent(ard_f <- filter_ard_hierarchical(ard, n == 2 & p < 0.05))
  expect_equal(nrow(ard_f), 45)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, sum(n) > 4))
  expect_equal(nrow(ard_f), 144)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, mean(n) > 4 | n > 3))
  expect_equal(nrow(ard_f), 108)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, any(n > 5 & TRTA == "Xanomeline High Dose")))
  expect_equal(nrow(ard_f), 90)
})

test_that("filter_ard_hierarchical() works with ard_stack_hierarchical_count() results", {
  withr::local_options(width = 200)

  ard <- ard_stack_hierarchical_count(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    over_variables = TRUE
  )

  expect_silent(ard_f <- filter_ard_hierarchical(ard, n > 10))
  expect_equal(nrow(ard_f), 32)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, sum(n) > 15))
  expect_equal(nrow(ard_f), 42)
})

test_that("filter_ard_hierarchical() returns only summary rows when all rows filtered out", {
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    include = "AETERM",
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID
  )

  expect_silent(ard_f <- filter_ard_hierarchical(ard, n > 200))
  expect_equal(
    ard_f,
    ard |> dplyr::filter(variable != "AETERM")
  )

  expect_true(all(ard_f$variable == "TRTA"))
})

test_that("filter_ard_hierarchical(keep_empty) works", {
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AEBODSYS, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID
  )

  # keep summary rows
  expect_silent(ard_f <- filter_ard_hierarchical(ard, sum(n) > 10, keep_empty = TRUE))
  expect_equal(nrow(ard_f), 270)

  # remove summary rows
  expect_silent(ard_f <- filter_ard_hierarchical(ard, sum(n) > 10))
  expect_equal(nrow(ard_f), 153)

  # all inner rows removed (only header rows remain)
  expect_silent(ard_f <- filter_ard_hierarchical(ard, sum(n) > 1000))
  expect_equal(nrow(ard_f), 9)

  ard_noincl <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AEBODSYS, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    include = AETERM
  )

  # no summary rows to remove
  expect_silent(ard_f <- filter_ard_hierarchical(ard_noincl, sum(n) > 10))
  expect_silent(ard_f_keep <- filter_ard_hierarchical(ard_noincl, sum(n) > 10, keep_empty = TRUE))
  expect_equal(nrow(ard_f), 72)
  expect_identical(ard_f, ard_f_keep)
})

test_that("filter_ard_hierarchical() works with only one variable in x", {
  ard_single <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = AETERM,
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID
  )

  expect_silent(ard_single <- filter_ard_hierarchical(ard_single, n > 20))
  expect_equal(nrow(ard_single), 15)
})

test_that("filter_ard_hierarchical() works when some variables not included in x", {
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    include = c(SEX, AETERM),
    over_variables = TRUE
  )

  expect_silent(filter_ard_hierarchical(ard, n > 10))
})

test_that("filter_ard_hierarchical() works with overall data", {
  ard_overall <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    over_variables = TRUE,
    overall = TRUE
  )

  expect_equal(
    ard_overall |>
      filter_ard_hierarchical(n > 5) |>
      nrow(),
    ard |>
      filter_ard_hierarchical(n > 5) |>
      nrow()
  )
})

test_that("filter_ard_hierarchical() error messaging works", {
  # invalid x input
  expect_snapshot(
    filter_ard_hierarchical(ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"), n > 10),
    error = TRUE
  )

  # invalid filter input
  expect_snapshot(
    filter_ard_hierarchical(ard, 10),
    error = TRUE
  )

  # invalid filter parameters
  expect_snapshot(
    filter_ard_hierarchical(ard, A > 5),
    error = TRUE
  )

  # invalid keep_empty input
  expect_snapshot(
    filter_ard_hierarchical(ard, n > 1, keep_empty = NULL),
    error = TRUE
  )
})
