test_that("rename_ard_columns() works", {
  # no variable level
  res_var <- ard_continuous(data = ADSL, by = TRT01A, variables = c(AGE, BMIBL, HEIGHTBL))

  res_rnm_var <- rename_ard_columns(res_var, all_ard_variables())

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

  res_rnm_multi <- rename_ard_columns(res_multi, all_ard_variables())

  expect_equal(nrow(res_multi), nrow(res_rnm_multi))

  res_multi_1 <- rename_ard_columns(res_multi, c(all_ard_groups(), all_ard_variables()))

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
      rename_ard_columns(c(
        variable, group1, group2_level,
        variable_level, group2, group1_level
      ))
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
      rename_ard_columns(c(variable, label)) |>
      dplyr::slice(1:20) |>
      as.data.frame()
  )

  # _level doesn't have a match
  res_var <- ard_continuous(data = ADSL, by = TRT01A, variables = c(AGE, BMIBL, HEIGHTBL))

  expect_snapshot(
    rename_ard_columns(
      res_var |> dplyr::select(-group1),
      "group1_level"
    )
  )
})
