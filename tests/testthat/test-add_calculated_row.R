test_that("add_calculated_row(x)", {
  expect_snapshot(
    ard_continuous(mtcars, variables = mpg) |>
      add_calculated_row(expr = max - min, stat_name = "range") |>
      apply_fmt_fn()
  )

  expect_snapshot(
    ard_continuous(mtcars, variables = mpg) |>
      add_calculated_row(
        expr =
          dplyr::case_when(
            mean > median ~ "Right Skew",
            mean < median ~ "Left Skew",
            .default = "Symmetric"
          ),
        stat_name = "skew"
      ) |>
      apply_fmt_fn()
  )
})


test_that("add_calculated_row(expr) messaging", {
  expect_snapshot(
    ard_continuous(mtcars, variables = mpg) |>
      add_calculated_row(expr = not_a_stat * 2, stat_name = "this_doesnt_work"),
    error = TRUE
  )
})

test_that("add_calculated_row(by) messaging", {
  expect_snapshot(
    ard_continuous(mtcars, variables = mpg, by = cyl) |>
      add_calculated_row(expr = max - min, stat_name = "range", by = "context"),
    error = TRUE
  )
})
