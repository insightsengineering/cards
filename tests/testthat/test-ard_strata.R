test_that("ard_strata() works", {
  expect_snapshot(
    ard_strata(
      ADSL,
      .by = ARM,
      .f = ~ ard_continuous(.x, variables = AGE)
    )
  )

  expect_snapshot(
    ard_strata(
      ADSL,
      .strata = ARM,
      .f = ~ ard_continuous(.x, variables = AGE, by = AGEGR1)
    )
  )

  expect_equal(
    ard_strata(ADSL, .by = ARM, .f = ~ ard_continuous(.x, by = c(SEX, AGEGR1), variables = AGE)) |>
      tidy_ard_column_order() |>
      tidy_ard_row_order(),
    ard_continuous(ADSL, by = c(SEX, AGEGR1, ARM), variables = AGE) |>
      tidy_ard_row_order()
  )
})

test_that("ard_strata(by,strata) when both empty", {
  expect_equal(
    ard_strata(ADSL, .f = ~ ard_continuous(.x, variables = AGE)),
    ard_continuous(ADSL, variables = AGE)
  )

  expect_equal(
    ard_strata(ADSL, .f = ~ ard_continuous(.x, by = ARM, variables = AGE)),
    ard_continuous(ADSL, by = ARM, variables = AGE)
  )
})

test_that("nest_for_ard retains strata in nested data", {
  df <- tibble::tibble(
    PARAMCD = rep(c("A", "B"), each = 2),
    VALUE = 1:4
  )

  nested <- nest_for_ard(df, strata = "PARAMCD")
  # Ensure 'PARAMCD' is in each nested data frame
  expect_true(all(purrr::map_lgl(nested$data, ~ "PARAMCD" %in% names(.x))))

  # Get the name of the outer strata column (e.g., "group1_level" or "PARAMCD")
  outer_strata_name <- setdiff(names(nested), "data")
})
