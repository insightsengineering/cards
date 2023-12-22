test_that("ard_dichotomous() works", {
  expect_error(
    ard_dich <-
      ard_dichotomous(
        mtcars |> dplyr::mutate(gear = factor(gear), am = as.logical(am)),
        variables = c("cyl", "am", "gear"),
        values = list(cyl = 4)
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
      dplyr::select(-context) |>
      flatten_ard(),
    ard_dich |>
      dplyr::filter(variable %in% "cyl", variable_level %in% 4) |>
      dplyr::select(-context) |>
      flatten_ard()
  )

  expect_equal(
    ard_categorical(
      mtcars |> dplyr::mutate(am = as.logical(am)),
      variables = am
    ) |>
      dplyr::filter(variable_level %in% TRUE) |>
      dplyr::select(-context) |>
      flatten_ard(),
    ard_dich |>
      dplyr::filter(variable %in% "am", variable_level %in% TRUE) |>
      dplyr::select(-context) |>
      flatten_ard()
  )

  expect_snapshot(
    ard_dich |>
      dplyr::select(-c(statistic_fmt_fn, warning, error)) |>
      as.data.frame()
  )
})


test_that("ard_dichotomous() works", {
  expect_snapshot(
    ard_dichotomous(
      mtcars,
      variables = c("cyl", "am", "gear"),
      values = list(cyl = letters)
    ),
    error = TRUE
  )

  expect_snapshot(
    ard_dichotomous(
      iris,
      variables = everything(),
      values = list(Species = "not_a_species")
    ),
    error = TRUE
  )

  expect_snapshot(
    ard_dichotomous(
      mtcars,
      variables = c("cyl", "am", "gear"),
      values = list(cyl = 100)
    ),
    error = TRUE
  )

})
