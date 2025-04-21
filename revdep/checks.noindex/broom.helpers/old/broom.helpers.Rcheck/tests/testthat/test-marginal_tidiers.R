test_that("tidy_margins()", {
  skip_on_cran()
  skip_if_not_installed("margins")

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_no_error(
    suppressWarnings(t <- tidy_margins(mod))
  )
  expect_error(
    tidy_margins(mod, exponentiate = TRUE)
  )
  expect_no_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_margins)
  )
  expect_equal(
    nrow(res),
    nrow(t) + 1 # due to adding ref row
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Average Marginal Effects"
  )
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_margins,
      add_pairwise_contrasts = TRUE
    )
  )
})

test_that("tidy_all_effects()", {
  skip_on_cran()
  skip_if_not_installed("effects")

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_no_error(
    t <- tidy_all_effects(mod)
  )
  expect_error(
    tidy_all_effects(mod, exponentiate = TRUE)
  )
  expect_no_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_all_effects)
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Predictions at the Mean"
  )
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_all_effects,
      add_pairwise_contrasts = TRUE
    )
  )
})

test_that("tidy_ggpredict()", {
  skip_on_cran()
  skip_if_not_installed("ggeffects")

  mod <- lm(Petal.Length ~ Petal.Width + Species, data = iris)
  expect_no_error(
    t <- tidy_ggpredict(mod)
  )
  expect_error(
    tidy_ggpredict(mod, exponentiate = TRUE)
  )
  expect_no_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_ggpredict)
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Predictions"
  )
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_ggpredict,
      add_pairwise_contrasts = TRUE
    )
  )
})

test_that("tidy_marginal_predictions()", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects")

  iris <- iris |> dplyr::arrange(dplyr::desc(Species))
  mod <- lm(Petal.Length ~ Petal.Width + Species + Sepal.Length, data = iris)
  expect_no_error(
    t <- tidy_marginal_predictions(mod)
  )
  expect_equal(t[t$variable == "Species", "term"], levels(iris$Species))

  mod <- lm(Petal.Length ~ Petal.Width * Species + Sepal.Length, data = iris)
  expect_no_error(
    t <- tidy_marginal_predictions(mod)
  )
  expect_error(
    tidy_marginal_predictions(mod, exponentiate = TRUE)
  )
  expect_no_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_marginal_predictions)
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Average Marginal Predictions"
  )
  expect_true(any(res$var_type == "interaction"))
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginal_predictions,
      add_pairwise_contrasts = TRUE
    )
  )

  expect_no_error(
    t <- tidy_marginal_predictions(mod, "no_interaction")
  )
  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginal_predictions,
      variables_list = "no_interaction"
    )
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_false(any(res$var_type == "interaction"))

  expect_no_error(
    t <- tidy_marginal_predictions(mod, newdata = "mean")
  )
  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginal_predictions,
      newdata = "mean"
    )
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Predictions at the Mean"
  )

  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginal_predictions,
      newdata = "balanced"
    )
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Predictions at Marginal Means"
  )
  expect_type(
    p <- plot_marginal_predictions(mod),
    "list"
  )
  expect_length(p, 2)
  expect_type(
    p <- plot_marginal_predictions(mod, variables_list = "no_interaction"),
    "list"
  )
  expect_length(p, 3)
})

test_that("tidy_avg_slopes()", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects")

  mod <- lm(Petal.Length ~ Petal.Width * Species + Sepal.Length, data = iris)
  expect_no_error(
    t <- tidy_avg_slopes(mod)
  )
  expect_error(
    tidy_avg_slopes(mod, exponentiate = TRUE)
  )
  expect_no_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_avg_slopes)
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Average Marginal Effects"
  )
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_avg_slopes,
      add_pairwise_contrasts = TRUE
    )
  )

  expect_no_error(
    t <- tidy_avg_slopes(mod, newdata = "mean")
  )
  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_avg_slopes,
      newdata = "mean"
    )
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Effects at the Mean"
  )

  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_avg_slopes,
      newdata = "balanced"
    )
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Effects at Marginal Means"
  )
})

test_that("tidy_marginal_contrasts()", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects")

  mod <- lm(Petal.Length ~ Petal.Width * Species + Sepal.Length, data = iris)
  expect_no_error(
    t <- tidy_marginal_contrasts(mod)
  )
  expect_error(
    tidy_marginal_contrasts(mod, exponentiate = TRUE)
  )
  expect_no_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_marginal_contrasts)
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Average Marginal Contrasts"
  )
  expect_true(any(res$var_type == "interaction"))
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginal_contrasts,
      add_pairwise_contrasts = TRUE
    )
  )

  expect_no_error(
    t <- tidy_marginal_contrasts(mod, "no_interaction")
  )
  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginal_contrasts,
      variables_list = "no_interaction"
    )
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_false(any(res$var_type == "interaction"))

  expect_no_error(
    t <- tidy_marginal_contrasts(mod, newdata = "mean")
  )
  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginal_contrasts,
      newdata = "mean"
    )
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Contrasts at the Mean"
  )

  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_marginal_contrasts,
      newdata = "balanced"
    )
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Contrasts at Marginal Means"
  )
})

test_that("tidy_avg_comparisons()", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects")

  mod <- lm(Petal.Length ~ Petal.Width * Species + Sepal.Length, data = iris)
  expect_no_error(
    t <- tidy_avg_comparisons(mod)
  )
  expect_error(
    tidy_avg_comparisons(mod, exponentiate = TRUE)
  )
  expect_no_error(
    res <- tidy_plus_plus(mod, tidy_fun = tidy_avg_comparisons)
  )
  expect_equal(
    nrow(res),
    nrow(t)
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Average Marginal Contrasts"
  )
  expect_error(
    tidy_plus_plus(
      mod,
      tidy_fun = tidy_avg_comparisons,
      add_pairwise_contrasts = TRUE
    )
  )

  expect_no_error(
    t <- tidy_avg_comparisons(mod, newdata = "mean"),
  )
  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_avg_comparisons,
      newdata = "mean"
    )
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Contrasts at the Mean"
  )

  expect_no_error(
    res <- tidy_plus_plus(
      mod,
      tidy_fun = tidy_avg_comparisons,
      newdata = "balanced"
    )
  )
  expect_equal(
    attr(res, "coefficients_label"),
    "Marginal Contrasts at Marginal Means"
  )
})

test_that("Marginal tidiers works with nnet::multinom() models", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  skip_if_not_installed("margins")
  skip_if_not_installed("effects")
  skip_if_not_installed("ggeffects")
  skip_if_not_installed("marginaleffects")

  suppressMessages(
    mod <- nnet::multinom(
      grade ~ stage + marker + age,
      data = gtsummary::trial,
      trace = FALSE
    )
  )

  # not supported: tidy_margins(mod)

  expect_no_error(
    res <- tidy_all_effects(mod)
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    suppressMessages(res <- tidy_ggpredict(mod))
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    res <- tidy_avg_slopes(mod)
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    res <- tidy_avg_comparisons(mod)
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    res <- tidy_marginal_predictions(mod)
  )
  expect_true("y.level" %in% names(res))

  expect_type(
    p <- plot_marginal_predictions(mod),
    "list"
  )
  expect_length(p, 3)

  expect_no_error(
    res <- tidy_marginal_contrasts(mod)
  )
  expect_true("y.level" %in% names(res))
})

test_that("Marginal tidiers works with MASS::polr() models", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  skip_if_not_installed("margins")
  skip_if_not_installed("effects")
  skip_if_not_installed("ggeffects")
  skip_if_not_installed("marginaleffects")

  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)

  expect_no_error(
    suppressMessages(res <- tidy_margins(mod))
  )
  # for margins, no result per y.level

  expect_no_error(
    suppressMessages(res <- tidy_all_effects(mod))
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    suppressMessages(res <- tidy_ggpredict(mod))
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    suppressMessages(res <- tidy_avg_slopes(mod))
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    suppressMessages(res <- tidy_avg_comparisons(mod))
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    suppressMessages(res <- tidy_marginal_predictions(mod))
  )
  expect_true("y.level" %in% names(res))

  expect_type(
    suppressMessages(p <- plot_marginal_predictions(mod)),
    "list"
  )
  expect_length(p, 3)

  expect_no_error(
    suppressMessages(res <- tidy_marginal_contrasts(mod))
  )
  expect_true("y.level" %in% names(res))
})

test_that("Marginal tidiers works with ordinal::clm() models", {
  skip_on_cran()
  skip_if_not_installed("ordinal")
  library(ordinal)

  skip_if_not_installed("margins")
  skip_if_not_installed("effects")
  skip_if_not_installed("ggeffects")
  skip_if_not_installed("marginaleffects")

  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine)

  # not supported: tidy_margins(mod)

  library(MASS)
  expect_no_error(
    res <- tidy_all_effects(mod)
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    suppressMessages(res <- tidy_ggpredict(mod))
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    res <- tidy_avg_slopes(mod)
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    res <- tidy_avg_comparisons(mod)
  )
  expect_true("y.level" %in% names(res))

  expect_no_error(
    res <- tidy_marginal_predictions(mod)
  )
  expect_true("y.level" %in% names(res))

  expect_type(
    p <- plot_marginal_predictions(mod),
    "list"
  )
  expect_length(p, 1)

  expect_no_error(
    res <- tidy_marginal_contrasts(mod)
  )
  expect_true("y.level" %in% names(res))
})
