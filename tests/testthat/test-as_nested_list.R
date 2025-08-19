test_that("as_nested_list() works", {
  expect_snapshot(
    ard_summary(mtcars, by = "cyl", variables = "hp") |>
      as_nested_list()
  )
})
