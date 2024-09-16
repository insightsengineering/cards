test_that("model_get_response_variable() works for basic models", {
  mod <- lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars)
  expect_equal(
    mod |> model_get_response_variable(),
    "hp"
  )

  mod <- glm(
    Survived ~ Class + Age + Sex,
    data = Titanic |> as.data.frame(),
    weights = Freq,
    family = binomial
  )
  expect_equal(
    mod |> model_get_response_variable(),
    "Survived"
  )

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_equal(
    mod |> model_get_response_variable(),
    "Petal.Length"
  )
})
