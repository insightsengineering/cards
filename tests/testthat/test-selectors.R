test_that("selectors work", {
  ard_testing <- ard_categorical(ADSL, by = ARM, variables = AGE)

  expect_equal(
    ard_testing |>
      dplyr::select(all_ard_groups()) |>
      names(),
    c("group1", "group1_level")
  )
  expect_equal(
    ard_testing |>
      dplyr::select(all_ard_groups(TRUE, FALSE)) |>
      names(),
    "group1"
  )
  expect_equal(
    ard_testing |>
      dplyr::select(all_ard_groups(FALSE, TRUE)) |>
      names(),
    "group1_level"
  )

  expect_equal(
    ard_testing |>
      dplyr::select(all_ard_variables()) |>
      names(),
    c("variable", "variable_level")
  )
  expect_equal(
    ard_testing |>
      dplyr::select(all_ard_variables(TRUE, FALSE)) |>
      names(),
    "variable"
  )
  expect_equal(
    ard_testing |>
      dplyr::select(all_ard_variables(FALSE, TRUE)) |>
      names(),
    "variable_level"
  )

  # test group selector works for 10+ groups
  expect_equal(
    suppressMessages(
      rep_len(list(mtcars[c("am", "vs")]), length.out = 11) |> dplyr::bind_cols()
    ) |>
      ard_categorical(
        variables = "vs...2",
        by = starts_with("am"),
        statistics = ~ categorical_variable_summary_fns("n")
      ) |>
      dplyr::select(all_ard_groups()) |>
      names() |>
      length(),
    22L
  )
})
