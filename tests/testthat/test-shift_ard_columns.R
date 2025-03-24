skip_if_not(is_pkg_installed("withr"))

test_that("rename_ard_columns() works", {
  withr::local_options(width = 220)
  # no variable level
  res_var <- ard_continuous(data = ADSL, by = TRT01A, variables = c(AGE, BMIBL, HEIGHTBL))

  res_rnm_var <- rename_ard_columns(res_var, all_ard_variables("names"))

  expect_equal(nrow(res_var), nrow(res_rnm_var))

  expect_snapshot(res_rnm_var |>
    dplyr::select(-c(fmt_fn, warning, error)) |>
    dplyr::slice(1:20) |>
    as.data.frame())


  # multiple variables and levels
  res_multi <- ard_stack(
    data = ADSL,
    .by = c(TRT01A, SEX),
    ard_categorical(variables = c(RACE, ETHNIC)),
    ard_continuous(variables = AGE)
  )

  res_rnm_multi <- rename_ard_columns(res_multi, all_ard_variables("names"))

  expect_equal(nrow(res_multi), nrow(res_rnm_multi))

  res_multi_1 <- rename_ard_columns(res_multi, c(all_ard_groups("names"), all_ard_variables("names")))

  # rename groups and variables
  expect_snapshot(
    res_multi_1 |>
      dplyr::select(-c(fmt_fn, warning, error)) |>
      dplyr::slice(1:20) |>
      as.data.frame()
  )

  # robust to order and NSE
  expect_equal(
    res_multi_1,
    res_multi |>
      rename_ard_columns(c(variable, group1, group2))
  )
})
