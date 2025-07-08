test_that("print.card() works", {
  expect_snapshot(
    ard_continuous(ADSL, by = "ARM", variables = "AGE")
  )

  expect_snapshot(
    ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
  )

  expect_snapshot(
    ard_continuous(ADSL, variables = "AGE", fmt_fun = AGE ~ list(~ \(x) round(x, 3)))
  )

  # checking the print of Dates
  expect_snapshot(
    ard_continuous(
      data = data.frame(x = seq(as.Date("2000-01-01"), length.out = 10L, by = "day")),
      variables = x,
      statistic = ~ continuous_summary_fns(c("min", "max", "sd"))
    ) |>
      dplyr::select(-fmt_fun)
  )

  # checking the print of a complex matrix statistic result
  expect_snapshot(
    bind_ard(
      ard_attributes(mtcars, variables = mpg),
      ard_continuous(
        mtcars,
        variables = mpg,
        statistic =
          ~ continuous_summary_fns(
            "mean",
            other_stats = list(vcov = \(x) lm(mpg ~ am, mtcars) |> vcov())
          )
      )
    )
  )
})

test_that("print.card(width) works", {
  withr::local_options(list(width = 150))

  expect_snapshot(
    ard_continuous(ADSL, by = "ARM", variables = "AGE")
  )

  expect_snapshot(
    ard_continuous(ADSL, by = c("ARM", "SEX"), variables = c("AGE", "BMIBL")) |>
      print(n = 5)
  )
})

test_that("print.card(n) works", {
  withr::local_options(list(width = 150))

  expect_snapshot(
    ard_continuous(ADSL, by = "ARM", variables = "AGE") |>
      print(n = 2)
  )

  expect_snapshot(
    ard_continuous(ADSL, by = "ARM", variables = "AGE") |>
      print(n = 20)
  )
})

test_that("print.card() messaging works", {
  withr::local_options(list(width = 150))

  expect_snapshot(
    print(ard_continuous(ADSL, variables = "AGE"), n = 1)
  )
})
