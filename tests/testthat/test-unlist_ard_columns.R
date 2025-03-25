test_that("unlist_ard_columns()", {
  expect_equal(
    ard_categorical(ADSL, variables = AGEGR1) |>
      unlist_ard_columns() |>
      dplyr::pull("stat") |>
      class(),
    "numeric"
  )

  expect_equal(
    ard_categorical(ADSL, variables = AGEGR1) |>
      unlist_ard_columns() |>
      dplyr::pull("variable_level") |>
      class(),
    "character"
  )

  expect_equal(
    ard_categorical(ADSL, variables = AGEGR1) |>
      unlist_ard_columns(columns = "error") |>
      dplyr::pull("error") |>
      unique(),
    NA
  )
})

test_that("unlist_ard_columns() messaging", {
  expect_message(
    ard_categorical(ADSL, variables = AGEGR1) |>
      dplyr::mutate(
        stat = ifelse(dplyr::row_number() == 1L, list(matrix(1:4)), stat)
      ) |>
      unlist_ard_columns(columns = "stat"),
    'Cannot unlist column'
  )
})
