test_that("process_selectors() works", {
  # works with a single argument
  expect_equal(
    {
      process_selectors(mtcars, variables = starts_with("a"))
      list(variables = variables)
    },
    list(variables = "am")
  )

  # works with more than on argument
  expect_equal(
    {
      process_selectors(mtcars, variables = starts_with("a"), by = "am")
      list(variables = variables, by = by)
    },
    list(variables = "am", by = "am")
  )

  # proper error messaging
  expect_snapshot(
    error = TRUE,
    process_selectors(mtcars, variables = not_a_column)
  )
})

test_that("process_formula_selectors() works", {
  # works with a single argument
  expect_equal(
    {
      process_formula_selectors(mtcars, variables = starts_with("a") ~ 1L)
      list(variables = variables)
    },
    list(variables = list(am = 1L))
  )

  # works with more than on argument
  # styler: off
  expect_equal({
    process_formula_selectors(mtcars, variables = starts_with("a") ~ 1L, by = list(am = 1L))
    list(variables = variables, by = by)},
    list(variables = list(am = 1L), by = list(am = 1L))
  )
  # styler: on
})

test_that("process_formula_selectors() error messaging", {
  expect_snapshot(
    process_formula_selectors(mtcars, variables = list(letters)),
    error = TRUE
  )
})

test_that("compute_formula_selector() selects the last assignment when multiple appear", {
  expect_snapshot(
    compute_formula_selector(
      data = mtcars[c("mpg", "hp")],
      x = list(everything() ~ "THE DEFAULT", mpg = "Special for MPG")
    )
  )


  # named list elements that are not in `data` are removed from returned result
  expect_equal(
    compute_formula_selector(
      data = mtcars[c("mpg", "hp")],
      x = list(everything() ~ "THE DEFAULT", not_present = "Special for MPG")
    ),
    list(mpg = "THE DEFAULT", hp = "THE DEFAULT")
  )
  expect_equal(
    compute_formula_selector(
      data = mtcars[c("mpg", "hp")],
      x = list(mpg = "THE DEFAULT", not_present = "Special for MPG")
    ),
    list(mpg = "THE DEFAULT")
  )
  expect_equal(
    compute_formula_selector(
      data = mtcars[c("mpg", "hp")],
      x = list(not_present = "Special for MPG")
    ),
    list(NAME = NULL) |> compact()
  )
})
