ttest_fn <- \(x, data, ...) t.test(x ~ data$ARM)[c("statistic", "p.value")]

test_that("ard_pairwise() works", {
  expect_silent(
    lst_ard <-
      ard_pairwise(
        ADSL,
        variable = ARM,
        .f = \(df) ard_complex(df, variables = AGE, statistic = ~ list(ttest = ttest_fn)),
        include = "Placebo" # only include comparisons to the "Placebo" group
      )
  )
  expect_length(lst_ard, 2L)

  expect_equal(
    lst_ard[["'Placebo' vs. 'Xanomeline High Dose'"]],
    ard_complex(
      ADSL |> dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")),
      variables = AGE,
      statistic = ~ list(ttest = ttest_fn)
    )
  )

  expect_equal(
    lst_ard[["'Placebo' vs. 'Xanomeline Low Dose'"]],
    ard_complex(
      ADSL |> dplyr::filter(ARM %in% c("Placebo", "Xanomeline Low Dose")),
      variables = AGE,
      statistic = ~ list(ttest = ttest_fn)
    )
  )
})

test_that("ard_pairwise(variable)", {
  # we get expected results with unobserved factor levels
  expect_silent(
    lst_ard <-
      data.frame(
        ARM = rep_len("Placebo", 20L) |> factor(levels = c("Placebo", "Unobserved Level")),
        AGE = 1:20
      ) |>
      ard_pairwise(
        variable = ARM,
        .f = \(df) ard_complex(df, variables = AGE, statistic = ~ list(ttest = ttest_fn))
      )
  )
  expect_equal(names(lst_ard), "'Placebo' vs. 'Unobserved Level'")
  expect_s3_class(lst_ard[[1]], "card")
  expect_equal(nrow(lst_ard[[1]]), 1L)
})

test_that("ard_pairwise(variable) messaging", {
  # only works with a single variable
  expect_snapshot(
    error = TRUE,
    ard_pairwise(
      ADSL,
      variable = c(ARM, AGEGR1),
      .f = \(df) ard_complex(df, variables = AGE, statistic = ~ list(ttest = ttest_fn))
    )
  )

  expect_snapshot(
    error = TRUE,
    ard_pairwise(
      ADSL,
      variable = NOT_A_COLUMN,
      .f = \(df) ard_complex(df, variables = AGE, statistic = ~ list(ttest = ttest_fn))
    )
  )
})

test_that("ard_pairwise(include)", {
  expect_silent(
    lst_ard <-
      ard_pairwise(
        ADSL,
        variable = ARM,
        .f = \(df) ard_complex(df, variables = AGE, statistic = ~ list(ttest = ttest_fn)),
        include = "Placebo" # only include comparisons to the "Placebo" group
      )
  )
  expect_equal(
    names(lst_ard),
    c("'Placebo' vs. 'Xanomeline High Dose'", "'Placebo' vs. 'Xanomeline Low Dose'")
  )
})

test_that("ard_pairwise(.f) messaging", {
  expect_snapshot(
    error = TRUE,
    ard_pairwise(ADSL, variable = ARM, .f = \(df) stop("I MADE THIS ERROR"))
  )
})

test_that("ard_pairwise(include) messaging", {
  # include is not a level of the variable
  expect_snapshot(
    error = TRUE,
    ard_pairwise(
      ADSL,
      variable = ARM,
      .f = \(df) ard_complex(df, variables = AGE, statistic = ~ list(ttest = ttest_fn)),
      include = "NOT_A_LEVEL"
    )
  )

  # include input is not a vector
  expect_snapshot(
    error = TRUE,
    ard_pairwise(
      ADSL,
      variable = ARM,
      .f = \(df) ard_complex(df, variables = AGE, statistic = ~ list(ttest = ttest_fn)),
      include = mtcars
    )
  )
})
