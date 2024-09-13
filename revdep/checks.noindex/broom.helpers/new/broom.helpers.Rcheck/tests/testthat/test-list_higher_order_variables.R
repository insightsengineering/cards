test_that("model_list_higher_order_variables() works for basic models", {
  mod <- lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars)
  expect_equal(
    mod |> model_list_higher_order_variables(),
    c("mpg", "factor(cyl)", "hp:disp")
  )

  mod <- glm(
    Survived ~ Class * Age + Sex:Class,
    data = Titanic |> as.data.frame(),
    weights = Freq,
    family = binomial
  )
  expect_equal(
    mod |> model_list_higher_order_variables(),
    c("Class:Age", "Class:Sex")
  )

  mod <- lm(Petal.Length ~ Petal.Width * Species * Sepal.Length, data = iris)
  expect_equal(
    mod |> model_list_higher_order_variables(),
    "Petal.Width:Species:Sepal.Length"
  )
})
