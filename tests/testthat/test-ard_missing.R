test_that("ard_missing() works", {
  expect_error(
    ard <- ard_missing(ADSL, by = "ARM", variables = "BMIBL"),
    NA
  )

  expect_snapshot(
    ard |>
      dplyr::select(-"fmt_fn") |>
      as.data.frame()
  )

  # confirm missing rate is correct
  expect_equal(
    ard |>
      dplyr::filter(stat_name %in% "p_miss") |>
      dplyr::pull(stat) |>
      unlist(),
    ADSL |>
      dplyr::mutate(BMIBL = is.na(BMIBL)) |>
      dplyr::summarise(
        .by = ARM,
        stat = mean(BMIBL)
      ) |>
      dplyr::pull(stat)
  )
})

test_that("ard_missing(stat_label) argument works", {
  # formula
  expect_snapshot(
    ard_missing(
      data = ADSL,
      by = "ARM",
      variables = c("AGE", "BMIBL"),
      stat_label = everything() ~ list(c("N_obs", "N_miss") ~ "N, miss")
    ) |>
      as.data.frame() |>
      dplyr::select(stat_name, stat_label) |>
      dplyr::filter(stat_name %in% c("N_obs", "N_miss")) |>
      unique()
  )

  # list
  expect_snapshot(
    ard_missing(
      data = ADSL,
      by = "ARM",
      variables = c("AGEGR1", "SEX"),
      stat_label = everything() ~ list(p_miss = "% miss", p_nonmiss = "% non miss")
    ) |>
      as.data.frame() |>
      dplyr::select(stat_name, stat_label) |>
      dplyr::filter(stat_name %in% c("p_miss", "p_nonmiss")) |>
      unique()
  )

  # variable-specific
  expect_snapshot(
    ard_missing(
      data = ADSL,
      by = "ARM",
      variables = c("AGE", "BMIBL"),
      stat_label = AGE ~ list(N_obs = "Number of Obs")
    ) |>
      as.data.frame() |>
      dplyr::select(variable, stat_name, stat_label) |>
      dplyr::filter(stat_name == "N_obs") |>
      unique()
  )
})

test_that("ard_missing() with grouped data works", {
  expect_equal(
    ADSL |>
      dplyr::group_by(ARM) |>
      ard_missing(variables = "BMIBL"),
    ard_missing(
      data = ADSL,
      by = "ARM",
      variables = "BMIBL"
    )
  )
})

test_that("ard_missing() follows ard structure", {
  expect_silent(
    ADSL |>
      dplyr::group_by(ARM) |>
      ard_missing(variables = "BMIBL") |>
      check_ard_structure(method = FALSE)
  )
})

test_that("ard_missing() errors with incomplete factor columns", {

  # First check output is fine when there is a valid factor variable
  expect_snapshot(
    mtcars |>
      dplyr::mutate(am = factor(am)) |>
      ard_missing(variables = am)
  )

  # Check error when factors have no levels
  # NOTE: Error only triggered when factor col with no levels is not the column
  # passed to variables in ard_missing
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = character(0))) |>
      ard_missing(variables = mpg)
  )

  # Check error when factor has NA level
  # NOTE: Error only triggered when factor col with na level is not the column
  # passed to variables in ard_missing
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = c(0, 1, NA), exclude = NULL)) |>
      ard_missing(variables = mpg)
  )

})
