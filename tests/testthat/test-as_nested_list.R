test_that("as_nested_list() works", {
  tbl <- ard_summary(mtcars, by = "cyl", variables = "hp")
  out <- as_nested_list(tbl)
  expect_type(out, "list")
  expect_named(out, "variable")

  expect_snapshot(
      as_nested_list(tbl)
  )
})
