test_that("tidy_disambiguate_terms() changes nothing for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_identify_variables()

  # no change by default
  res2 <- res |> tidy_disambiguate_terms()
  expect_equivalent(res, res2)
  expect_false("original_term" %in% names(res2))
})

test_that("tidy_disambiguate_terms() works for mixed models", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  skip_if_not_installed("broom.mixed")
  res <- mod |>
    tidy_and_attach() |>
    tidy_disambiguate_terms(sep = ".")
  expect_equivalent(
    res$term,
    c(
      "(Intercept)", "Days", "Subject.sd__(Intercept)",
      "Subject.cor__(Intercept).Days",
      "Subject.sd__Days", "Residual.sd__Observation"
    )
  )
  expect_true("original_term" %in% names(res))

  res <- mod |>
    tidy_and_attach() |>
    tidy_disambiguate_terms(sep = "_")
  expect_equivalent(
    res$term,
    c(
      "(Intercept)", "Days", "Subject_sd__(Intercept)",
      "Subject_cor__(Intercept).Days",
      "Subject_sd__Days", "Residual_sd__Observation"
    )
  )
})



test_that("test tidy_disambiguate_terms() checks", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("broom.mixed")
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  # expect an error if no model attached
  expect_error(mod |> broom.mixed::tidy() |> tidy_disambiguate_terms())

  # could be apply twice (no error but a message)
  expect_error(
    mod |>
      tidy_and_attach() |>
      tidy_disambiguate_terms() |>
      tidy_disambiguate_terms(),
    NA
  )
  expect_message(
    mod |>
      tidy_and_attach(tidy_fun = broom::tidy) |>
      tidy_disambiguate_terms() |>
      tidy_disambiguate_terms()
  )
  expect_message(
    mod |>
      tidy_and_attach(tidy_fun = broom::tidy) |>
      tidy_disambiguate_terms() |>
      tidy_disambiguate_terms(quiet = TRUE),
    NA
  )
})
