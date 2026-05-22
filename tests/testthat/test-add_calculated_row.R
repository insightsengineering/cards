test_that("add_calculated_row(x)", {
  tbl <- ard_summary(mtcars, variables = mpg)
  out <- add_calculated_row(tbl, expr = max - min, stat_name = "range")
  expect_named(out, c("variable", "context", "stat_name", "stat_label", "stat", "fmt_fun",
                      "warning", "error"))
  expect_s3_class(out, "card")

  expect_setequal(out$stat_label,
                  c("N", "Mean", "SD", "Median", "Q1", "Q3", "Min", "Max", "range"
  ))

  expect_snapshot(out)
  out2 <- add_calculated_row(
    tbl,
    expr =
      dplyr::case_when(
        mean > median ~ "Right Skew",
        mean < median ~ "Left Skew",
        .default = "Symmetric"
      ),
    stat_name = "skew"
  )
  expect_setequal(out2$stat_label,
                  c("N", "Mean", "SD", "Median", "Q1", "Q3", "Min", "Max", "skew"
  ))

  expect_snapshot(out2)

})

test_that("add_calculated_row(expr) errors when a variable is not present", {
  tbl <- ard_summary(mtcars, variables = mpg)
  expect_error(add_calculated_row(tbl, expr = not_a_stat * 2, stat_name = "this_doesnt_work"),
               "calculating the new statistic")
  expect_snapshot(
      add_calculated_row(tbl, expr = not_a_stat * 2, stat_name = "this_doesnt_work"),
    error = TRUE
  )
})

test_that("add_calculated_row(by) messaging", {
  tbl <- ard_summary(mtcars, variables = mpg, by = cyl)
  expect_error(add_calculated_row(tbl, expr = max - min, stat_name = "range", by = "context"),
               "Duplicate statistics")
  expect_snapshot(
      add_calculated_row(tbl, expr = max - min, stat_name = "range", by = "context"),
    error = TRUE
  )
})
