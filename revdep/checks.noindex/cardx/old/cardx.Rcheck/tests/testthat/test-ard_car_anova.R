skip_if_not(
  is_pkg_installed(c("broom.helpers", "car", "parameters"), reference_pkg = "cardx")
)

test_that("ard_car_anova() works", {
  # works for a generic case
  expect_error(
    glm_ard_car_anova <-
      suppressWarnings(glm(vs ~ factor(cyl) + factor(am), data = mtcars, family = binomial)) |>
      ard_car_anova(test.statistic = "Wald"),
    NA
  )
  expect_equal(nrow(glm_ard_car_anova), 6L)
  expect_snapshot(glm_ard_car_anova)
})

test_that("ard_car_anova() messaging", {
  expect_snapshot(
    error = TRUE,
    ard_car_anova(mtcars)
  )
})
