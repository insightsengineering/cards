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
