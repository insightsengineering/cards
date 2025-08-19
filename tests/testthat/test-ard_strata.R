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
  df <- data.frame(
    USUBJID = 1:12,
    TREAT = rep(c("TREAT", "PLACEBO"), times = 6),
    PARAMCD = rep(c("PARAM1", "PARAM2"), each = 6),
    AVALC = c(
      "Yes", "No", "Yes",           # PARAM1
      "Yes", "Yes", "No",           # PARAM1
      "low", "medium", "high",      # PARAM2
      "low", "low", "medium"        # PARAM2
    ))
  nested <- nest_for_ard(df, strata = "PARAMCD")
  # Ensure 'PARAMCD' is in each nested data frame
  expect_true(all(map_lgl(nested$data, ~ "PARAMCD" %in% names(.x))))
})

test_that("ard_strata computes stats for parameter specific strata", {
  df <- data.frame(
    USUBJID = 1:12,
    TREAT = rep(c("TREAT", "PLACEBO"), times = 6),
    PARAMCD = rep(c("PARAM1", "PARAM2"), each = 6),
    AVALC = c(
      "Yes", "No", "Yes",           # PARAM1
      "Yes", "Yes", "No",           # PARAM1
      "low", "medium", "high",      # PARAM2
      "low", "low", "medium"        # PARAM2
    ))
  param_levels <-
    list(
      PARAM1 = c("Yes", "No"),
      PARAM2 = c("Zero", "Low", "Medium", "High")
    )

  tbl <- ard_strata(
    df,
    .strata = PARAMCD,
    .f = \(.x) {
      param <- .x[["PARAMCD"]][1]  # Get current PARAMCD
      .x |>
        dplyr::mutate(
          AVALC = factor(AVALC, levels = param_levels[[param]])
        ) |>
        ard_tabulate(
          by = TREAT,
          variables = AVALC,
        )
    }
  )
  expect_snapshot(as.data.frame(tbl)[1:25,])
})
