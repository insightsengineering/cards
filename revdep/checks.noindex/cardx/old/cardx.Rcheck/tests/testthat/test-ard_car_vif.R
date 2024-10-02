skip_if_not(is_pkg_installed("car", reference_pkg = "cardx"))

test_that("ard_car_vif() works", {
  expect_snapshot(
    lm(AGE ~ ARM + SEX, data = cards::ADSL) |>
      ard_car_vif() |>
      as.data.frame()
  )

  expect_snapshot(
    lm(AGE ~ BMIBL + EDUCLVL, data = cards::ADSL) |>
      ard_car_vif() |>
      as.data.frame()
  )
})

test_that("ard_car_vif() appropriate errors are given for model with only 1 term", {
  expect_equal(
    lm(AGE ~ ARM, data = cards::ADSL) |>
      ard_car_vif() |>
      dplyr::select(error) %>%
      is.null(),
    FALSE
  )
})


test_that("ard_vif() issues friendly messaging for incorrect object passed in/can't get terms of model", {
  expect_snapshot(
    error = TRUE,
    cards::ADSL |> ard_car_vif()
  )
})

test_that("ard_car_vif() follows ard structure", {
  expect_silent(
    lm(AGE ~ ARM + SEX, data = cards::ADSL) |>
      ard_car_vif() |>
      cards::check_ard_structure(method = FALSE)
  )
})
