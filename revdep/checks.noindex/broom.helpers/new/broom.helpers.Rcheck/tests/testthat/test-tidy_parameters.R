test_that("tidy_parameters() works for basic models", {
  skip_if_not_installed("parameters")
  mod <- lm(Petal.Length ~ Petal.Width, iris)
  expect_error(
    mod |> tidy_parameters(),
    NA
  )
  expect_error(
    mod |> tidy_plus_plus(tidy_fun = tidy_parameters),
    NA
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  expect_error(
    mod |> tidy_parameters(),
    NA
  )
  expect_error(
    res1 <- mod |> tidy_plus_plus(tidy_fun = tidy_parameters),
    NA
  )
  expect_error(
    res2 <- mod |> tidy_plus_plus(tidy_fun = tidy_parameters, conf.level = .80),
    NA
  )
  expect_false(identical(res1$conf.low, res2$conf.low))
  expect_error(
    res <- mod |> tidy_plus_plus(tidy_fun = tidy_parameters, conf.int = FALSE),
    NA
  )
  expect_false("conf.low" %in% res)
})

test_that("tidy_with_broom_or_parameters() works for basic models", {
  skip_if_not_installed("parameters")
  mod <- lm(Petal.Length ~ Petal.Width, iris)
  expect_error(
    mod |> tidy_with_broom_or_parameters(),
    NA
  )

  expect_error(
    suppressWarnings("not a model" |> tidy_with_broom_or_parameters())
  )
})
