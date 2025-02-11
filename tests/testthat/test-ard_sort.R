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

test_that("ard_sort() works", {
  withr::local_options(width = 200)

  expect_silent(ard <- ard_sort(ard))
  expect_snapshot(ard |> dplyr::select(all_ard_groups(), all_ard_variables()) |> print(n = 50))
})

test_that("ard_sort(sort = 'descending') works", {
  # descending count (default)
  expect_silent(ard <- ard_sort(ard))
  expect_equal(
    ard |>
      dplyr::filter(variable == "SEX") |>
      dplyr::select(variable_level) |>
      dplyr::distinct() |>
      dplyr::pull(variable_level) |>
      unlist(),
    c("F", "M")
  )
  expect_equal(
    ard |>
      dplyr::filter(variable == "RACE") |>
      dplyr::select(all_ard_groups("levels"), -"group1_level", all_ard_variables()) |>
      dplyr::distinct() |>
      dplyr::pull(variable_level) |>
      unlist(),
    c("WHITE", "BLACK OR AFRICAN AMERICAN", "WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE")
  )
  expect_equal(
    ard |>
      dplyr::filter(variable == "AETERM") |>
      dplyr::select(all_ard_groups("levels"), -"group1_level", all_ard_variables()) |>
      dplyr::distinct() |>
      dplyr::pull(variable_level) |>
      unlist(),
    c(
      "APPLICATION SITE PRURITUS", "ERYTHEMA", "APPLICATION SITE ERYTHEMA", "DIARRHOEA", "APPLICATION SITE PRURITUS",
      "ERYTHEMA", "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "DIARRHOEA", "APPLICATION SITE PRURITUS",
      "APPLICATION SITE ERYTHEMA", "ERYTHEMA", "DIARRHOEA", "ATRIOVENTRICULAR BLOCK SECOND DEGREE",
      "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "ERYTHEMA"
    )
  )
})

test_that("ard_sort(sort = 'alphanumeric') works", {
  expect_silent(ard <- ard_sort(ard, sort = "alphanumeric"))

  expect_equal(
    ard |>
      dplyr::filter(variable == "SEX") |>
      dplyr::select(variable_level) |>
      dplyr::distinct() |>
      dplyr::pull(variable_level) |>
      unlist(),
    sort(c("F", "M"))
  )
  expect_equal(
    ard |>
      dplyr::filter(variable == "RACE") |>
      dplyr::select(all_ard_groups("levels"), -"group1_level", all_ard_variables()) |>
      dplyr::distinct() |>
      dplyr::pull(variable_level) |>
      unlist(),
    c("BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN", "WHITE")
  )
  expect_equal(
    ard |>
      dplyr::filter(variable == "AETERM") |>
      dplyr::select(all_ard_groups("levels"), -"group1_level", all_ard_variables()) |>
      dplyr::distinct() |>
      dplyr::pull(variable_level) |>
      unlist(),
    c(
      "APPLICATION SITE PRURITUS", "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "DIARRHOEA", "ERYTHEMA",
      "APPLICATION SITE ERYTHEMA", "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "ERYTHEMA",
      "APPLICATION SITE PRURITUS", "DIARRHOEA", "ERYTHEMA", "APPLICATION SITE ERYTHEMA", "APPLICATION SITE PRURITUS",
      "ATRIOVENTRICULAR BLOCK SECOND DEGREE", "DIARRHOEA", "ERYTHEMA"
    )
  )
})

test_that("ard_sort() works when there is no overall row in x", {
  ard_no_overall <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    over_variables = FALSE
  )

  # sort = 'descending'
  expect_silent(ard_no_overall <- ard_sort(ard_no_overall))
  expect_equal(
    ard_no_overall |> dplyr::select(all_ard_groups(), all_ard_variables()),
    ard |>
      ard_sort() |>
      dplyr::select(all_ard_groups(), all_ard_variables()) |>
      dplyr::filter(variable != "..ard_hierarchical_overall..")
  )

  # sort = 'alphanumeric'
  expect_silent(ard_no_overall <- ard_sort(ard_no_overall, sort = "alphanumeric"))
  expect_equal(
    ard_no_overall |> dplyr::select(all_ard_groups(), all_ard_variables()),
    ard |>
      ard_sort("alphanumeric") |>
      dplyr::select(all_ard_groups(), all_ard_variables()) |>
      dplyr::filter(variable != "..ard_hierarchical_overall..")
  )
})

test_that("ard_sort() works with only one variable in x", {
  ard_single <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = AETERM,
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    over_variables = TRUE
  )

  # sort = 'descending'
  expect_silent(ard_single <- ard_sort(ard_single))
  expect_equal(
    ard_single |>
      dplyr::filter(variable == "AETERM") |>
      dplyr::pull(variable_level) |>
      unlist() |>
      unique(),
    c(
      "APPLICATION SITE PRURITUS", "ERYTHEMA", "APPLICATION SITE ERYTHEMA", "DIARRHOEA",
      "ATRIOVENTRICULAR BLOCK SECOND DEGREE"
    )
  )

  # sort = 'alphanumeric'
  expect_silent(ard_single <- ard_sort(ard_single, sort = "alphanumeric"))
  expect_equal(
    ard_single |>
      dplyr::filter(variable == "AETERM") |>
      dplyr::pull(variable_level) |>
      unlist() |>
      unique(),
    sort(unique(ADAE_subset$AETERM))
  )
})

test_that("ard_sort() works when some variables not included in x", {
  ard_incl <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    include = c(SEX, AETERM),
    over_variables = TRUE
  )

  expect_equal(
    ard_incl |>
      ard_sort() |>
      dplyr::select(all_ard_groups(), all_ard_variables()),
    ard |>
      ard_sort() |>
      dplyr::filter(variable != "RACE") |>
      dplyr::select(all_ard_groups(), all_ard_variables()),
    ignore_attr = TRUE
  )
})

test_that("ard_sort() works when sorting using p instead of n", {
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    statistic = everything() ~ "p"
  )

  expect_silent(ard_p <- ard_sort(ard))

  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    statistic = everything() ~ "p"
  )
})

test_that("ard_sort() error messaging works", {
  # invalid x input
  expect_snapshot(
    ard_sort(ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")),
    error = TRUE
  )

  # invalid sort input
  expect_snapshot(
    ard_sort(ard, sort = "no_sorting"),
    error = TRUE
  )

  # no n or p stat in ARD
  ard <- ard_stack_hierarchical(
    data = ADAE_subset,
    variables = c(SEX, RACE, AETERM),
    by = TRTA,
    denominator = cards::ADSL |> dplyr::mutate(TRTA = ARM),
    id = USUBJID,
    statistic = everything() ~ "N"
  )

  expect_snapshot(
    ard_sort(ard),
    error = TRUE
  )
})
