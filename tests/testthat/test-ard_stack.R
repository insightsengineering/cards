test_that("ard_stack() works", {
  # with by variable
  expect_error(
    ard1 <- ard_stack(
      data = mtcars,
      .by = "cyl",
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
      ard_categorical(data = mtcars, variables = "cyl"),
      .order = TRUE
    ),
    ignore_function_env = TRUE
  )

  # check equivalency NSE
  expect_equal(
    ard1,
    ard_stack(
      data = mtcars,
      .by = cyl,
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
      .by = all_of(by),
      ard_continuous(variables = all_of(var_cont)),
      ard_dichotomous(variables = all_of(var_cat))
    )
  )

  # without by variable
  expect_error(
    ard2 <- ard_stack(
      data = mtcars,
      .by = NULL,
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
      ard_continuous(variables = all_of(var_cont)),
      ard_dichotomous(variables = all_of(var_cat))
    )
  )
})

test_that("ard_stack() adding overalls", {
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      .by = "cyl",
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
      ard_continuous(data = mtcars, variables = "mpg"),
      ard_dichotomous(data = mtcars, variables = "vs"),
      ard_categorical(data = mtcars, variables = "cyl"),
      .update = TRUE,
      .order = TRUE
    )
  )
})



test_that("ard_stack() adding missing/attributes", {
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      .by = "cyl",
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
      ard_missing(data = mtcars, by = "cyl", variables = c("mpg", "vs")),
      ard_categorical(data = mtcars, variables = "cyl"),
      ard_attributes(mtcars, variables = c("mpg", "vs", "cyl")),
      .update = TRUE,
      .order = TRUE
    )
  )

  # including `.overall=TRUE`
  expect_error(
    ard_test_overall <- ard_stack(
      data = mtcars,
      .by = "cyl",
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs"),
      .missing = TRUE,
      .overall = TRUE,
      .attributes = TRUE
    ),
    NA
  )

  expect_equal(
    ard_test_overall,
    bind_ard(
      ard_continuous(data = mtcars, by = "cyl", variables = "mpg"),
      ard_dichotomous(data = mtcars, by = "cyl", variables = "vs"),
      ard_missing(data = mtcars, by = "cyl", variables = c("mpg", "vs")),
      ard_continuous(data = mtcars, variables = "mpg"),
      ard_dichotomous(data = mtcars, variables = "vs"),
      ard_categorical(data = mtcars, variables = "cyl"),
      ard_missing(data = mtcars, variables = c("mpg", "vs")),
      ard_attributes(mtcars, variables = c("mpg", "vs", "cyl")),
      .update = TRUE,
      .order = TRUE
    )
  )
})


test_that("ard_stack() .shuffle argument", {
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      .by = "cyl",
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
      ard_categorical(data = mtcars, variables = "cyl"),
      .order = TRUE
    ) |>
      shuffle_ard()
  )


  # with overalls
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      .by = "cyl",
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs"),
      .shuffle = TRUE,
      .overall = TRUE
    ),
    NA
  )

  expect_equal(
    ard_test,
    bind_ard(
      ard_continuous(data = mtcars, by = "cyl", variables = "mpg"),
      ard_dichotomous(data = mtcars, by = "cyl", variables = "vs"),
      ard_continuous(data = mtcars, variables = "mpg"),
      ard_dichotomous(data = mtcars, variables = "vs"),
      ard_categorical(data = mtcars, variables = "cyl")
    ) |>
      shuffle_ard()
  )
})


test_that("ard_stack() adding total N", {
  expect_equal(
    ard_stack(
      mtcars,
      .by = am,
      ard_continuous(variables = mpg),
      .total_n = TRUE
    ) |>
      tail(n = 1) |>
      dplyr::select(-all_ard_groups(), -all_ard_variables("levels")),
    ard_total_n(mtcars)
  )
})

test_that("ard_stack() works with namespaced functions", {
  expect_equal(
    ard_stack(
      data = mtcars,
      cards::ard_continuous(variables = "mpg")
    ),
    ard_stack(
      data = mtcars,
      ard_continuous(variables = "mpg")
    )
  )
})

test_that("ard_stack() messaging", {
  withr::local_options(list(width = 150))
  expect_snapshot(
    ard_stack(
      data = mtcars,
      ard_continuous(variables = "mpg"),
      .overall = TRUE
    ) |>
      head(1L)
  )

  # by argument doesn't include period in front
  expect_snapshot(
    error = TRUE,
    ard_stack(ADSL, by = "ARM", ard_continuous(variables = AGE))
  )
})

test_that("ard_stack() complex call error", {
  withr::local_options(list(width = 150))
  expect_snapshot(
    {
      complex_call <- list()
      complex_call$ard_continuous <- ard_continuous
      ard_stack(
        data = mtcars,
        .by = am,
        complex_call$ard_continuous(variables = "mpg"),
      )
    },
    error = TRUE
  )
})

test_that("ard_stack() follows ard structure", {
  expect_silent(
    ard_stack(
      data = mtcars,
      .by = "cyl",
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs")
    ) |>
      check_ard_structure(method = FALSE)
  )
})

test_that("ard_stack(.by) messaging", {
  withr::local_options(list(width = 150))
  mtcars2 <- mtcars
  mtcars2$am[1] <- NA
  mtcars2$vs[1] <- NA
  expect_snapshot(
    mtcars2 |>
      ard_stack(
        ard_continuous(variables = "mpg", statistic = ~ continuous_summary_fns("N")),
        .by = c(am, vs),
        .total_n = TRUE,
        .overall = TRUE
      ) |>
      dplyr::filter(stat_name %in% "N")
  )

  mtcars3 <- mtcars
  mtcars3$am[1] <- NA
  mtcars3$vs[2] <- NaN
  expect_snapshot(
    mtcars3 |>
      ard_stack(
        ard_continuous(variables = "mpg", statistic = ~ continuous_summary_fns("N")),
        .by = c(am, vs),
        .total_n = TRUE,
        .overall = TRUE
      ) |>
      dplyr::filter(stat_name %in% "N")
  )
})

test_that("ard_stack() .by_stats argument", {
  # by stats for 1 variable
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      .by = "cyl",
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs"),
      .by_stats = TRUE
    ),
    NA
  )

  expect_equal(
    ard_test,
    bind_ard(
      ard_continuous(data = mtcars, by = "cyl", variables = "mpg"),
      ard_dichotomous(data = mtcars, by = "cyl", variables = "vs"),
      ard_categorical(data = mtcars, variables = "cyl"),
      .update = TRUE,
      .order = TRUE
    )
  )

  # by stats for 2 variables
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      .by = c("am", "cyl"),
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs"),
      .by_stats = TRUE
    ),
    NA
  )

  expect_equal(
    ard_test,
    bind_ard(
      ard_continuous(data = mtcars, c("am", "cyl"), variables = "mpg"),
      ard_dichotomous(data = mtcars, c("am", "cyl"), variables = "vs"),
      ard_categorical(data = mtcars, variables = "am"),
      ard_categorical(data = mtcars, variables = "cyl"),
      .update = TRUE,
      .order = TRUE
    )
  )

  # no by stats
  expect_error(
    ard_test <- ard_stack(
      data = mtcars,
      .by = c("am", "cyl"),
      ard_continuous(variables = "mpg"),
      ard_dichotomous(variables = "vs"),
      .by_stats = FALSE
    ),
    NA
  )

  expect_equal(
    ard_test,
    bind_ard(
      ard_continuous(data = mtcars, by = c("am", "cyl"), variables = "mpg"),
      ard_dichotomous(data = mtcars, by = c("am", "cyl"), variables = "vs"),
      .update = TRUE,
      .order = TRUE
    )
  )
})
