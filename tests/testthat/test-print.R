test_that("print.card() works", {
  expect_snapshot(
    ard_continuous(ADSL, by = "ARM", variables = "AGE")
  )

  expect_snapshot(
    ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
  )

  expect_snapshot(
    ard_continuous(ADSL, variables = "AGE", fmt_fn = AGE ~ list(~ \(x) round(x, 3)))
  )

  # checking the print of Dates
  expect_snapshot(
    ard_continuous(
      data = data.frame(x = seq(as.Date("2000-01-01"), length.out = 10L, by = "day")),
      variables = x,
      statistics = ~ continuous_variable_summary_fns(c("min", "max", "sd"))
    )
  )

  # checking the print of a complex matrix statistic result
  expect_snapshot(
    bind_ard(
      ard_attributes(mtcars, variables = mpg),
      ard_continuous(
        mtcars,
        variables = mpg,
        statistics =
          ~ continuous_variable_summary_fns(
            "mean",
            other_stats = list(vcov = \(x) lm(mpg ~ am, mtcars) |> vcov())
          )
      )
    )
  )
})
