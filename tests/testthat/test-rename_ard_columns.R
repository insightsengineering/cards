test_that("rename_ard_columns() works", {
  # no variable level
  res_var <- ard_continuous(data = ADSL, by = TRT01A, variables = c(AGE, BMIBL, HEIGHTBL))

  res_rnm_var <- rename_ard_columns(res_var, "variable")

  expect_equal(nrow(res_var), nrow(res_rnm_var))

  expect_snapshot(res_rnm_var |>
                    dplyr::select(-c(fmt_fn, warning, error)) |>
                    as.data.frame())


  # multiple variables and levels
  res_multi <- ard_stack(
    data = ADSL,
    .by = c(TRT01A, SEX),
    ard_categorical(variables = c(RACE, ETHNIC)),
    ard_continuous(variables = AGE)
  )

  res_rnm_multi <- rename_ard_columns(res_multi, "variable")

  expect_equal(nrow(res_multi), nrow(res_rnm_multi))

  res_multi_1 <- res_multi |>
    rename_ard_columns("group1") |>
    rename_ard_columns("group2") |>
    rename_ard_columns("variable")

  # rename groups and variables
  expect_snapshot(
      res_multi_1 |>
        dplyr::select(-c(fmt_fn, warning, error)) |>
        as.data.frame()
  )

  # robust to order and NSE
  expect_equal(
    res_multi_1,
    res_multi |>
      rename_ard_columns(group2) |>
      rename_ard_columns(variable) |>
      rename_ard_columns(group1)
  )

  # rename variables after shuffle
  # include case where one of the variables is already present as a group
  res_shuffle <- ard_stack(
    data = ADSL,
    .by = TRT01A,
    ard_categorical(variables = c(RACE, ETHNIC)),
    ard_continuous(variables = AGE),
    .shuffle = TRUE
  )

  expect_snapshot(
    res_shuffle |>
      rename_ard_columns("variable", col_lev = "label")|>
      as.data.frame()
  )
})
