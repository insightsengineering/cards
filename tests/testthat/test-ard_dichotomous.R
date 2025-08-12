test_that("ard_tabulate(value) works", {
  expect_error(
    ard_dich <-
      ard_tabulate(
        mtcars |> dplyr::mutate(gear = factor(gear), am = as.logical(am)),
        variables = c("cyl", "am", "gear"),
        value = c(maximum_variable_value(mtcars[c("am", "gear")]),  list(cyl = 4))
      ),
    NA
  )
  expect_snapshot(class(ard_dich))

  expect_equal(
    ard_tabulate(
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
    ard_tabulate(
      mtcars |> dplyr::mutate(am = as.logical(am)),
      variables = am,
      value = am ~ 1
    ) |>
      dplyr::filter(variable_level %in% TRUE) |>
      dplyr::select(-context),
    ard_dich |>
      dplyr::filter(variable %in% "am", variable_level %in% TRUE) |>
      dplyr::select(-context)
  )

  ## line added to fix failing snapshot test on ubuntu-latest (devel)
  ## TODO: resolve after release of R-devel
  skip_if_not(package_version(paste(R.version$major, R.version$minor, sep = ".")) <= package_version("4.5.0"))

  expect_snapshot(
    ard_dich |>
      dplyr::select(-c(fmt_fun, warning, error)) |>
      as.data.frame()
  )
})


test_that("ard_tabulate(value) errors are correct", {
  expect_snapshot(
    ard_tabulate(
      mtcars,
      variables = c("cyl", "am", "gear"),
      value = list(cyl = letters)
    ),
    error = TRUE
  )

  expect_snapshot(
    ard_tabulate(
      iris,
      variables = everything(),
      value = list(Species = "not_a_species")
    ),
    error = TRUE
  )

  expect_snapshot(
    ard_tabulate(
      mtcars,
      variables = c("cyl", "am", "gear"),
      value = list(cyl = 100)
    ),
    error = TRUE
  )
})


test_that("ard_tabulate(value) with grouped data works", {
  expect_equal(
    mtcars |>
      dplyr::group_by(vs) |>
      ard_tabulate(variables = c(cyl, am), value = list(cyl = 4, am = 1)),
    ard_tabulate(
      data = mtcars,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = 4, am = 1)
    )
  )
})

test_that("ard_tabulate(value) follows ard structure", {
  expect_silent(
    mtcars |>
      dplyr::group_by(vs) |>
      ard_tabulate(variables = c(cyl, am), value = list(cyl = 4, am = 1)) |>
      check_ard_structure(method = FALSE)
  )
})

test_that("ard_tabulate(value) errors with incomplete factor columns", {
  # Check error when factors have no levels
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = character(0))) |>
      ard_tabulate(
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
      ard_tabulate(
        variables = c(cyl, am),
        value = list(cyl = 4)
      )
  )
})
