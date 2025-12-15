test_that("check_ard_structure() works", {
  expect_snapshot(
    ard_summary(ADSL, variables = "AGE") |>
      dplyr::mutate(stat = unlist(stat)) |>
      dplyr::select(-error) |>
      structure(class = "data.frame") |>
      check_ard_structure()
  )
})


test_that("check_ard_structure() does not error if the tested dataset has none of the expected variables", {
  expect_snapshot(
  expect_no_error(
      check_ard_structure(data.frame(badname = 3))
  )
  )
})

test_that("check_ard_structure() errors when flagged appropriately", {

  expect_snapshot(
    check_ard_structure(data.frame(badname = 3), error_on_fail = TRUE),
    error = TRUE
  )

  nolist <- dplyr::tibble(
    variable = "AGE",
    stat_name = c("method", "mean"),
    stat_label = c("N", "Mean"),
    stat = c(10, 0.5),
    fmt_fun = replicate(2, list()),
    warning = replicate(2, list()),
    error = replicate(2, list())
  )
  nolist <- structure(nolist, class = c("card", class(nolist)))

  expect_snapshot(
    check_ard_structure(nolist, error_on_fail = TRUE),
    error = TRUE
  )
  novariable <- dplyr::tibble(
    stat_name = c("method", "mean"),
    stat_label = c("N", "Mean"),
    stat = list(10, 0.5),
    fmt_fun = replicate(2, list()),
    warning = replicate(2, list()),
    error = replicate(2, list())
  )
  novariable <- structure(novariable, class = c("card", class( novariable)))

  expect_snapshot(
    check_ard_structure(  novariable , error_on_fail = TRUE),
    error = TRUE
  )

  wrongorder <- dplyr::tibble(
    variable = "AGE",
    stat_name = c("method", "mean"),
    stat_label = c("N", "Mean"),
    fmt_fun = replicate(2, list()),
    warning = replicate(2, list()),
    stat = list(10, 0.5),
    error = replicate(2, list())
  )
  wrongorder <- structure(wrongorder, class = c("card", class(wrongorder)))

  expect_snapshot(
    check_ard_structure(wrongorder, error_on_fail = TRUE),
    error = TRUE
  )

})
