test_that("select_helpers: all_*()", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod_tidy <- tidy_and_attach(mod)

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_categorical())$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "trt", "grade")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_categorical(dichotomous = FALSE))$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "grade")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_continuous())$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "age")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_dichotomous())$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "trt")
  )

  expect_equal(
    tidy_select_variables(mod_tidy, include = all_interaction())$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)", "age:trt")
  )
})

test_that("select_helpers: tidy_plus_plus", {
  skip_on_cran()

  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod2 <- glm(response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.sum,
      grade = contr.poly,
      trt = contr.helmert
    )
  )
  mod3 <- glm(
    response ~ stage + grade + trt + factor(death),
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, 3), grade = contr.treatment(3, 2),
      trt = contr.treatment(2, 2), "factor(death)" = matrix(c(-3, 2))
    )
  )

  expect_equal(
    tidy_plus_plus(mod3, include = all_contrasts("treatment"))$variable |>
      na.omit() |>
      unique(),
    c("stage", "grade", "trt")
  )

  expect_equal(
    tidy_plus_plus(mod3, include = all_contrasts("other"))$variable |>
      na.omit() |>
      unique(),
    c("factor(death)")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_contrasts())$variable |>
      na.omit() |>
      unique(),
    c("trt", "grade")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_categorical())$variable |>
      na.omit() |>
      unique(),
    c("trt", "grade")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_contrasts("treatment"))$variable |>
      na.omit() |>
      unique(),
    c("trt", "grade")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_continuous())$variable |>
      na.omit() |>
      unique(),
    c("age")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_dichotomous())$variable |>
      na.omit() |>
      unique(),
    c("trt")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_interaction())$variable |>
      na.omit() |>
      unique(),
    c("age:trt")
  )

  expect_equal(
    tidy_plus_plus(mod, include = all_intercepts(), intercept = TRUE)$variable |>
      na.omit() |>
      unique(),
    c("(Intercept)")
  )

  expect_equal(
    tidy_plus_plus(mod,
      add_header_rows = TRUE,
      show_single_row = all_dichotomous()
    )$variable %in% "trt" |>
      sum(),
    1L
  )

  skip_if_not_installed("emmeans")
  expect_equal(
    tidy_plus_plus(mod2, include = all_contrasts("sum"))$variable |>
      na.omit() |>
      unique(),
    c("stage")
  )

  expect_equal(
    tidy_plus_plus(mod2, include = all_contrasts("poly"))$variable |>
      na.omit() |>
      unique(),
    c("grade")
  )

  expect_equal(
    tidy_plus_plus(mod2, include = all_contrasts("helmert"))$variable |>
      na.omit() |>
      unique(),
    c("trt")
  )

  skip_on_cran()
  skip_if_not_installed("lme4")
  mod3 <- lme4::lmer(age ~ stage + (stage | grade) + (1 | grade), gtsummary::trial)
  res <- mod3 |> tidy_plus_plus(
    tidy_fun = broom.mixed::tidy,
    include = all_ran_pars()
  )
  expect_equal(
    res$term,
    c(
      "grade.sd__(Intercept)", "grade.cor__(Intercept).stageT2",
      "grade.cor__(Intercept).stageT3", "grade.cor__(Intercept).stageT4",
      "grade.sd__stageT2", "grade.cor__stageT2.stageT3", "grade.cor__stageT2.stageT4",
      "grade.sd__stageT3", "grade.cor__stageT3.stageT4", "grade.sd__stageT4",
      "grade.1.sd__(Intercept)", "Residual.sd__Observation"
    )
  )
  res <- mod3 |> tidy_plus_plus(
    tidy_fun = broom.mixed::tidy,
    include = all_ran_vals()
  )
  expect_equal(res |> nrow(), 0L)
})

test_that("select_helpers: tidy_add_header_rows", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod_tidy <- tidy_and_attach(mod)

  expect_equal(
    tidy_add_header_rows(mod_tidy, show_single_row = all_dichotomous())$variable %in% "trt" |>
      sum(),
    1L
  )
})

test_that("select_helpers: tidy_add_variable_labels", {
  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  mod_tidy <- tidy_and_attach(mod)

  expect_no_error(
    tidy_add_variable_labels(mod_tidy, labels = where(is.numeric) ~ "NUMERIC")
  )

  expect_equal(
    tidy_add_variable_labels(mod_tidy,
      labels = list(
        `(Intercept)` ~ "b0",
        age ~ "AGE",
        trt ~ "Drug",
        "grade" ~ "Grade",
        contains("age:") ~ "Interaction"
      )
    ) |>
      dplyr::pull(var_label) |>
      unique(),
    c("b0", "AGE", "Drug", "Grade", "Interaction")
  )
})

test_that("select helpers are consistent with gtsummary", {
  skip_on_cran()
  skip_if_not_installed("gtsummary")

  mod <- glm(response ~ age * trt + grade, gtsummary::trial, family = binomial)
  x <- mod |>
    tidy_and_attach() |>
    tidy_identify_variables() |>
    tidy_add_contrasts() |>
    scope_tidy()

  expect_equal(
    x |> dplyr::select(broom.helpers::all_categorical()) |> colnames(),
    x |> dplyr::select(gtsummary::all_categorical()) |> colnames()
  )

  expect_equal(
    x |> dplyr::select(broom.helpers::all_continuous()) |> colnames(),
    x |> dplyr::select(gtsummary::all_continuous()) |> colnames()
  )

  expect_equal(
    x |> dplyr::select(broom.helpers::all_contrasts("treatment")) |> colnames(),
    x |> dplyr::select(gtsummary::all_contrasts("treatment")) |> colnames()
  )

  expect_equal(
    x |> dplyr::select(broom.helpers::all_dichotomous()) |> colnames(),
    x |> dplyr::select(gtsummary::all_dichotomous()) |> colnames()
  )

  expect_equal(
    x |> dplyr::select(broom.helpers::all_interaction()) |> colnames(),
    x |> dplyr::select(gtsummary::all_interaction()) |> colnames()
  )

  expect_equal(
    x |> dplyr::select(broom.helpers::all_intercepts()) |> colnames(),
    x |> dplyr::select(gtsummary::all_intercepts()) |> colnames()
  )
})
