test_that("ard_dichotomous() works", {
  expect_error(
    ard_dich <-
      ard_dichotomous(
        mtcars |> dplyr::mutate(gear = factor(gear), am = as.logical(am)),
        variables = c("cyl", "am", "gear"),
        value = list(cyl = 4)
      ),
    NA
  )
  expect_snapshot(class(ard_dich))

  expect_equal(
    ard_categorical(
      mtcars,
      variables = cyl
    ) |>
      dplyr::filter(variable_level %in% 4) |>
      dplyr::select(-context),
    ard_dich |>
      dplyr::filter(variable %in% "cyl", variable_level %in% 4) |>
      dplyr::select(-context)
  )

  expect_equal(
    ard_categorical(
      mtcars |> dplyr::mutate(am = as.logical(am)),
      variables = am
    ) |>
      dplyr::filter(variable_level %in% TRUE) |>
      dplyr::select(-context),
    ard_dich |>
      dplyr::filter(variable %in% "am", variable_level %in% TRUE) |>
      dplyr::select(-context)
  )

  expect_snapshot(
    ard_dich |>
      dplyr::select(-c(fmt_fn, warning, error)) |>
      as.data.frame()
  )
})


test_that("ard_dichotomous() works", {
  expect_snapshot(
    ard_dichotomous(
      mtcars,
      variables = c("cyl", "am", "gear"),
      value = list(cyl = letters)
    ),
    error = TRUE
  )

  expect_snapshot(
    ard_dichotomous(
      iris,
      variables = everything(),
      value = list(Species = "not_a_species")
    ),
    error = TRUE
  )

  expect_snapshot(
    ard_dichotomous(
      mtcars,
      variables = c("cyl", "am", "gear"),
      value = list(cyl = 100)
    ),
    error = TRUE
  )
})


test_that("ard_dichotomous() with grouped data works", {
  expect_equal(
    mtcars |>
      dplyr::group_by(vs) |>
      ard_dichotomous(variables = c(cyl, am), value = list(cyl = 4)),
    ard_dichotomous(
      data = mtcars,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = 4)
    )
  )
})

test_that("ard_dichotomous() follows ard structure", {
  expect_silent(
    mtcars |>
      dplyr::group_by(vs) |>
      ard_dichotomous(variables = c(cyl, am), value = list(cyl = 4)) |>
      check_ard_structure(method = FALSE)
  )
})

test_that("ard_dichotomous() errors with incomplete factor columns", {
  # Check error when factors have no levels
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = character(0))) |>
      ard_dichotomous(
        variables = c(cyl, vs),
        by = am,
        value = list(cyl = 4)
      )
  )

  # Check error when factor has NA level
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = c(0, 1, NA), exclude = NULL)) |>
      ard_dichotomous(
        variables = c(cyl, am),
        value = list(cyl = 4)
      )
  )
})
