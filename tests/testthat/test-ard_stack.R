test_that("ard_stack() works", {
  # with by variable
  expect_error(
    ard1 <- ard_stack(
      data = mtcars,
      by = "cyl",
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs")
    ),
    NA
  )

  expect_equal(
    ard1,
    bind_ard(
      ard_continuous(data = mtcars, by = "cyl", variables = "mpg"),
      ard_dichotomous(data = mtcars, by = "cyl", variables = "vs"),
      ard_categorical(data = mtcars, variables = "cyl", statistic = everything() ~ categorical_summary_fns("N")),
      .order = TRUE
    ),
    ignore_function_env = TRUE
  )

  # check equivalency NSE
  expect_equal(
    ard1,
    ard_stack(
      data = mtcars,
      by = cyl,
      ard_continuous(variables = mpg),
      ard_dichotomous(variables = vs)
    ),
    ignore_function_env = TRUE
  )

  # check equivalency tidyselect
  mtcars2 <- mtcars
  by <- "cyl"
  var_cont <- "mpg"
  var_cat <- "vs"
  expect_equal(
    ard1,
    ard_stack(
      data = mtcars2,
      by = all_of(by),
      ard_continuous(variables = all_of(var_cont)),
      ard_dichotomous(variables = all_of(var_cat))
    )
  )

  # without by variable
  expect_error(
    ard2 <- ard_stack(
      data = mtcars,
      by = NULL,
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs")
    ),
    NA
  )

  expect_equal(
    ard2,
    bind_ard(
      ard_continuous(data = mtcars, variables = "mpg"),
      ard_dichotomous(data = mtcars, variables = "vs"),
      .order = TRUE
    ),
    ignore_function_env = TRUE
  )


  expect_equal(
    ard2,
    ard_stack(
      data = mtcars2,
      by = NULL,
      ard_continuous(variables = all_of(var_cont)),
      ard_dichotomous(variables = all_of(var_cat))
    )
  )
})

test_that("ard_stack() adding overalls", {
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      by = "cyl",
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs"),
      .overall = TRUE
    ),
    NA
  )


  expect_equal(
    ard_test,
    bind_ard(
      ard_continuous(data = mtcars, by = "cyl", variables = "mpg"),
      ard_dichotomous(data = mtcars, by = "cyl", variables = "vs"),
      ard_categorical(data = mtcars, variables = "cyl", statistic = everything() ~ categorical_summary_fns("N")),
      ard_continuous(data = mtcars, variables = "mpg"),
      ard_dichotomous(data = mtcars, variables = "vs"),
      .update = TRUE,
      .order = TRUE
    )
  )
})


test_that("ard_stack() adding missing/attributes", {
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      by = "cyl",
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs"),
      .missing = TRUE,
      .attributes = TRUE
    ),
    NA
  )

  expect_equal(
    ard_test,
    bind_ard(
      ard_continuous(data = mtcars, by = "cyl", variables = "mpg"),
      ard_dichotomous(data = mtcars, by = "cyl", variables = "vs"),
      ard_categorical(data = mtcars, variables = "cyl", statistic = everything() ~ categorical_summary_fns("N")),
      ard_missing(data = mtcars, variables = c("cyl", "mpg", "vs")),
      ard_attributes(data = mtcars, variables = c("cyl", "mpg", "vs")),
      .update = TRUE,
      .order = TRUE
    )
  )
})


test_that("ard_stack() .shuffle argument", {
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      by = "cyl",
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs"),
      .shuffle = TRUE
    ),
    NA
  )

  expect_equal(
    ard_test,
    bind_ard(
      ard_continuous(data = mtcars, by = "cyl", variables = "mpg"),
      ard_dichotomous(data = mtcars, by = "cyl", variables = "vs"),
      ard_categorical(data = mtcars, variables = "cyl", statistic = everything() ~ categorical_summary_fns("N")),
      .order = TRUE
    ) |>
      shuffle_ard()
  )
})
