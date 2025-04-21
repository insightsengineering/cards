skip_if_not(is_pkg_installed("survey"))

test_that("ard_total_n.survey.design() works", {
  expect_snapshot(
    survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
      ard_total_n()
  )
})

test_that("ard_total_n.survey.design() follows ard structure", {
  expect_silent(
    survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
      ard_total_n() |>
      cards::check_ard_structure(method = FALSE)
  )
})
