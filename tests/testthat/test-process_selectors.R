test_that("process_selectors() works", {
  # works with a single argument
  expect_equal({
    process_selectors(mtcars, variables = starts_with("a"))
    list(variables = variables)},
    list(variables = "am")
  )

  # works with more than on argument
  expect_equal({
    process_selectors(mtcars, variables = starts_with("a"), by = "am")
    list(variables = variables, by = by)},
    list(variables = "am", by = "am")
  )
})

test_that("process_formula_selectors() works", {
  # works with a single argument
  expect_equal({
    process_formula_selectors(mtcars, variables = starts_with("a") ~ 1L)
    list(variables = variables)},
    list(variables = list(am = 1L))
  )

  # works with more than on argument
  expect_equal({
    process_formula_selectors(mtcars, variables = starts_with("a") ~ 1L, by = list(am = 1L))
    list(variables = variables, by = by)},
    list(variables = list(am = 1L), by = list(am = 1L))
  )
})

test_that("process_formula_selectors() error messaging", {
  expect_snapshot(
    process_formula_selectors(mtcars, variables = list(letters)),
    error = TRUE
  )
})
