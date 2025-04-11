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
    "Cannot unlist column"
  )
})

test_that("unlist_ard_columns(fct_as_chr)", {
  # check that a mixed-type column has factors converted to character by default.
  expect_true(
    cards::ADSL |>
      dplyr::mutate(ARM = factor(ARM)) |>
      ard_stack(
        ard_continuous(variables = AGE),
        .by = ARM
      ) |>
      unlist_ard_columns() |>
      dplyr::pull("group1_level") |>
      is.character()
  )

  # check fct_to_chr = FALSE
  expect_true(
    cards::ADSL |>
      dplyr::mutate(ARM = factor(ARM)) |>
      ard_stack(
        ard_continuous(variables = AGE),
        .by = ARM
      ) |>
      unlist_ard_columns(fct_as_chr = FALSE) |>
      dplyr::pull("group1_level") |>
      is.integer()
  )
})
