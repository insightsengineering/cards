test_that("tidy_select_variables() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_identify_variables()

  # no change by default
  res2 <- res |> tidy_select_variables()
  expect_equal(res, res2)

  # include
  res2 <- res |> tidy_select_variables(include = "stage")
  expect_equal(
    res2$variable,
    c("(Intercept)", "stage", "stage", "stage")
  )
  res2 <- res |> tidy_select_variables(include = c("grade", "trt"))
  expect_equal(
    res2$variable,
    c("(Intercept)", "grade", "grade", "trt")
  )

  res2 <- res |> tidy_select_variables(include = c("trt", "grade"))
  expect_equal(
    res2$variable,
    c("(Intercept)", "trt", "grade", "grade")
  )

  res2 <- res |> tidy_select_variables(include = c(trt, grade, dplyr::everything()))
  expect_equal(
    res2$variable,
    c("(Intercept)", "trt", "grade", "grade", "stage", "stage", "stage")
  )

  # select and de-select
  expect_equal(
    res |> tidy_select_variables(include = stage),
    res |> tidy_select_variables(include = -c(grade, trt))
  )

  # tidyselect fns
  expect_equal(
    res |> tidy_select_variables(include = contains("tage")),
    res |> tidy_select_variables(include = stage)
  )

  # no error when none selected
  expect_no_error(
    res |> tidy_select_variables(include = starts_with("zzzzzzz"))
  )
  expect_no_error(
    res |> tidy_select_variables(include = -everything())
  )
  expect_no_error(
    res |> tidy_select_variables(include = where(is.character))
  )

  # interaction
  mod <- glm(response ~ stage + grade * trt, gtsummary::trial, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_identify_variables()
  res2 <- res |> tidy_select_variables(include = c(trt, grade, dplyr::everything()))
  expect_equal(
    res2$variable,
    c(
      "(Intercept)", "trt", "grade", "grade", "stage", "stage", "stage",
      "grade:trt", "grade:trt"
    )
  )
})


test_that("test tidy_select_variables() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod |> broom::tidy() |> tidy_select_variables())

  # could be apply twice (no error)
  expect_no_error(
    mod |> tidy_and_attach() |> tidy_select_variables() |> tidy_select_variables()
  )
})
