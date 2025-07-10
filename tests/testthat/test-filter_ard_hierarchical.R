skip_on_cran()

ADAE_subset <- cards::ADAE |>
  dplyr::filter(AETERM %in% unique(cards::ADAE$AETERM)[1:5])

ard <- ard_stack_hierarchical(
  data = ADAE_subset,
  variables = c(SEX, RACE, AETERM),
  by = TRTA,
  denominator = cards::ADSL,
  id = USUBJID,
  over_variables = TRUE,
  overall = TRUE
)

test_that("filter_ard_hierarchical() works", {
  withr::local_options(width = 200)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, n > 10))
  expect_snapshot(ard_f)
  expect_equal(nrow(ard_f), 45)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, p > 0.05))
  expect_equal(nrow(ard_f), 234)
})

test_that("filter_ard_hierarchical() works with non-standard filters", {
  expect_silent(ard_f <- filter_ard_hierarchical(ard, n == 2 & p < 0.05))
  expect_equal(nrow(ard_f), 63)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, sum(n) > 4))
  expect_equal(nrow(ard_f), 144)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, mean(n) > 4 | n > 3))
  expect_equal(nrow(ard_f), 117)

  expect_silent(ard_f <- filter_ard_hierarchical(ard, any(n > 3 & TRTA == "Xanomeline High Dose")))
  expect_equal(nrow(ard_f), 108)

  expect_silent(
    ard_f <- filter_ard_hierarchical(
      ard,
      any(n > 5 & TRTA == "Xanomeline High Dose")
    )
  )
  expect_equal(nrow(ard_f), 90)
})

test_that("filter_ard_hierarchical() works with column-specific filters", {
  # test overall stat derivations (when overall=FALSE) are equal to stats when overall=TRUE
  ard_o <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
    over_variables = TRUE,
    overall = FALSE
  )

  # difference between n's in columns 2 and 3 > 1 (one-sided)
  expect_message(ard_f <- filter_ard_hierarchical(ard, n_2 - n_3 > 1))
  expect_equal(nrow(ard_f), 63)

  # difference between n's in columns 2 and 3 > 1 (absolute)
  expect_message(ard_f <- filter_ard_hierarchical(ard, abs(n_2 - n_3) > n_1))
  expect_equal(nrow(ard_f), 144)

  # overall prevalence across row group > 30%
  expect_silent(ard_f <- filter_ard_hierarchical(ard, p_overall > 0.3))
  # p_overall calculated correctly
  expect_identical(
    ard_f,
    ard |> filter_ard_hierarchical(sum(n) / sum(N) > 0.3),
  )
  # derived p_overall equal to p_overall coming from overall=TRUE
  expect_identical(
    ard_f,
    ard_o |> filter_ard_hierarchical(p_overall > 0.3)
  )

  # overall prevalence across row group > 15
  expect_silent(ard_f <- filter_ard_hierarchical(ard, n_overall > 15))
  # n_overall calculated correctly
  expect_identical(
    ard_f,
    ard |> filter_ard_hierarchical(sum(n) > 15)
  )
  # derived n_overall equal to n_overall coming from overall=TRUE
  expect_identical(
    ard_f,
    ard_o |> filter_ard_hierarchical(n_overall > 15)
  )

  # p_overall equal to n_overall / N_overall from overall=TRUE
  expect_silent(ard_f <- filter_ard_hierarchical(ard, n_overall / N_overall == p_overall))
  # derived p_overall equal to derived n_overall / N_overall
  expect_identical(
    ard_f,
    ard_o |> filter_ard_hierarchical(n_overall / N_overall == p_overall)
  )

  # check for number of rows
  expect_silent(ard_f <- filter_ard_hierarchical(ard, p_overall <= 0.1))
  expect_equal(nrow(ard_f), 108)

  # column-wise n statistic equal to previous derivation with column name specified (both still work)
  expect_message(ard_f <- filter_ard_hierarchical(ard, n_2 > 5))
  expect_identical(
    ard_f,
    ard |> filter_ard_hierarchical(any(n > 5 & TRTA == "Xanomeline High Dose"))
  )

  # column-wise p statistics equal to previous derivation with column names specified (both still work)
  expect_message(ard_f <- filter_ard_hierarchical(ard, p_2 > 0.15 | p_3 > 0.2))
  expect_identical(
    ard_f,
    ard |>
      filter_ard_hierarchical(any(p > 0.15 & TRTA == "Xanomeline High Dose") | any(p > 0.2 & TRTA == "Xanomeline Low Dose"))
  )
})

test_that("filter_ard_hierarchical() works with ard_stack_hierarchical_count() results", {
  withr::local_options(width = 200)

  ard <- ard_stack_hierarchical_count(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    over_variables = TRUE
  )

  expect_silent(ard_f <- filter_ard_hierarchical(ard, n > 10))
  expect_equal(nrow(ard_f), 39)

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

test_that("filter_ard_hierarchical(var) works", {
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(AESOC, AEDECOD, AETOXGR),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID
  )

  # default uses the correct variable
  expect_identical(
    filter_ard_hierarchical(ard, n > 5, var = AETOXGR),
    filter_ard_hierarchical(ard, n > 5)
  )

  # works with `var` specified
  expect_silent(ard_f <- filter_ard_hierarchical(ard, sum(n) > 15, var = AEDECOD))
  expect_equal(nrow(ard_f), 189)

  # works with first hierarchy variable
  expect_silent(ard_f <- filter_ard_hierarchical(ard, sum(n) > 50, var = AESOC))
  expect_equal(nrow(ard_f), 108)

  # works with no `by` variable
  ard_noby <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(AESOC, AEDECOD, AETOXGR),
    denominator = cards::ADSL,
    id = USUBJID
  )
  expect_silent(ard_f <- filter_ard_hierarchical(ard_noby, sum(n) > 10, var = AEDECOD))
  expect_equal(nrow(ard_f), 63)
})

test_that("filter_ard_hierarchical(keep_empty) works", {
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AEBODSYS, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID
  )

  # keep summary rows
  expect_silent(
    ard_f <- filter_ard_hierarchical(ard, sum(n) > 10, keep_empty = TRUE)
  )
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
    denominator = cards::ADSL,
    id = USUBJID,
    include = AETERM
  )

  # no summary rows to remove
  expect_silent(ard_f <- filter_ard_hierarchical(ard_noincl, sum(n) > 10))
  expect_silent(
    ard_f_keep <- filter_ard_hierarchical(
      ard_noincl,
      sum(n) > 10,
      keep_empty = TRUE
    )
  )
  expect_equal(nrow(ard_f), 72)
  expect_identical(ard_f, ard_f_keep)
})

test_that("filter_ard_hierarchical() works with only one variable in x", {
  ard_single <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = AETERM,
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID
  )

  expect_silent(ard_single <- filter_ard_hierarchical(ard_single, n > 20))
  expect_equal(nrow(ard_single), 18)
})

test_that("filter_ard_hierarchical() works when some variables not included in x", {
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
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
    denominator = cards::ADSL,
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
    filter_ard_hierarchical(
      ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"),
      n > 10
    ),
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

  # invalid var input
  expect_snapshot(
    filter_ard_hierarchical(ard, n > 1, var = "A"),
    error = TRUE
  )
  expect_snapshot(
    filter_ard_hierarchical(ard, n > 1, var = c(SEX, RACE)),
    error = TRUE
  )

  # invalid var input - not in include
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    include = c(SEX, AETERM),
    denominator = cards::ADSL,
    id = USUBJID,
    over_variables = TRUE
  )
  expect_snapshot(
    filter_ard_hierarchical(ard, n > 1, var = RACE),
    error = TRUE
  )

  # invalid keep_empty input
  expect_snapshot(
    filter_ard_hierarchical(ard, n > 1, keep_empty = NULL),
    error = TRUE
  )

  ard_stat_miss <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    statistic = ~"p"
  )

  # unavailable filter statistic
  expect_snapshot(
    filter_ard_hierarchical(ard_stat_miss, n_1 > 1),
    error = TRUE
  )

  # p_overall not valid
  expect_snapshot(
    filter_ard_hierarchical(ard_stat_miss, p_overall > 0.1),
    error = TRUE
  )
})
