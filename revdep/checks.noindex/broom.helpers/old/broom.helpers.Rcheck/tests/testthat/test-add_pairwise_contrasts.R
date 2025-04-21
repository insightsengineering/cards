test_that("tidy_add_pairwise_contrasts() works for glm", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  skip_if_not_installed("gtsummary")

  mod <- glm(response ~ stage + trt, gtsummary::trial, family = binomial)

  res <- mod |>
    tidy_and_attach() |>
    tidy_add_pairwise_contrasts()
  expect_equal(
    res$term,
    c(
      "(Intercept)", "T2 - T1", "T3 - T1", "T3 - T2", "T4 - T1",
      "T4 - T2", "T4 - T3", "Drug B - Drug A"
    )
  )

  res <- mod |>
    tidy_and_attach(exponentiate = TRUE) |>
    tidy_add_pairwise_contrasts()
  expect_equal(
    res$term,
    c(
      "(Intercept)", "T2 / T1", "T3 / T1", "T3 / T2", "T4 / T1",
      "T4 / T2", "T4 / T3", "Drug B / Drug A"
    )
  )
  expect_equal(
    round(res$estimate, digits = 2),
    c(0.48, 0.62, 1.12, 1.82, 0.82, 1.33, 0.73, 1.24)
  )
  expect_equal(
    round(res$conf.low, digits = 2),
    c(0.25, 0.2, 0.36, 0.56, 0.27, 0.42, 0.23, 0.67)
  )

  res <- mod |>
    tidy_and_attach(exponentiate = TRUE, conf.level = .9) |>
    tidy_add_pairwise_contrasts(
      variables = stage,
      keep_model_terms = TRUE,
      pairwise_reverse = FALSE
    )
  expect_equal(
    res$term,
    c(
      "(Intercept)", "stageT2", "stageT3", "stageT4", "T1 / T2",
      "T1 / T3", "T1 / T4", "T2 / T3", "T2 / T4", "T3 / T4", "trtDrug B"
    )
  )
  expect_equal(
    round(res$conf.low, digits = 2),
    c(0.27, 0.3, 0.54, 0.4, 0.6, 0.33, 0.46, 0.19, 0.27, 0.49, 0.74)
  )

  res <- mod |>
    tidy_plus_plus(exponentiate = TRUE, add_pairwise_contrasts = TRUE)
  expect_equal(
    res$term,
    c(
      "T2 / T1", "T3 / T1", "T3 / T2", "T4 / T1", "T4 / T2", "T4 / T3",
      "Drug B / Drug A"
    )
  )

  res1 <- mod |>
    tidy_plus_plus(add_pairwise_contrasts = TRUE)
  res2 <- mod |>
    tidy_plus_plus(add_pairwise_contrasts = TRUE, contrasts_adjust = "none")
  expect_false(identical(res1, res2))
})
