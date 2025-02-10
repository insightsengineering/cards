test_that("rename_ard_groups_shift()", {
  # no errors when no grouping variables
  expect_equal(
    ard_continuous(ADSL, variables = AGE) |>
      rename_ard_groups_shift(),
    ard_continuous(ADSL, variables = AGE)
  )

  # works under normal circumstances
  expect_snapshot(
    ard_continuous(ADSL, variables = AGE, by = c(SEX, ARM)) |>
      rename_ard_groups_shift(shift = 1L) |>
      dplyr::select(all_ard_groups()) %>%
      `[`(1L, )
  )
})

test_that("rename_ard_groups_shift() messaging", {
  expect_snapshot(
    ard_continuous(ADSL, variables = AGE, by = c(SEX, ARM)) |>
      rename_ard_groups_shift(shift = -1L) |>
      dplyr::select(all_ard_groups()) %>%
      `[`(1L, )
  )
})

test_that("rename_ard_groups_reverse()", {
  # no errors when no grouping variables
  expect_equal(
    ard_continuous(ADSL, variables = AGE) |>
      rename_ard_groups_reverse(),
    ard_continuous(ADSL, variables = AGE)
  )

  # works under normal circumstances
  expect_snapshot(
    ard_continuous(ADSL, variables = AGE, by = c(SEX, ARM)) |>
      rename_ard_groups_reverse() |>
      dplyr::select(all_ard_groups()) %>%
      `[`(1L, )
  )
})
