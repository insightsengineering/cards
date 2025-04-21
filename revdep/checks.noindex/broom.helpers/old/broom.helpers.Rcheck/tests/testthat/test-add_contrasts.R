test_that("tidy_add_contrast() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts,
    c(
      NA, "contr.treatment", "contr.treatment", "contr.treatment",
      "contr.treatment", "contr.treatment", "contr.treatment"
    )
  )
  expect_equal(
    res$contrasts_type,
    c(
      NA, "treatment", "treatment", "treatment", "treatment", "treatment",
      "treatment"
    )
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.sum, grade = contr.helmert, trt = contr.SAS)
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts,
    c(
      NA, "contr.sum", "contr.sum", "contr.sum", "contr.helmert",
      "contr.helmert", "contr.SAS"
    )
  )
  expect_equal(
    res$contrasts_type,
    c(NA, "sum", "sum", "sum", "helmert", "helmert", "treatment")
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.poly, grade = contr.treatment, trt = matrix(c(-3, 2)))
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts,
    c(
      NA, "contr.poly", "contr.poly", "contr.poly", "contr.treatment",
      "contr.treatment", "custom"
    )
  )
  expect_equal(
    res$contrasts_type,
    c(NA, "poly", "poly", "poly", "treatment", "treatment", "other")
  )

  mod <- glm(
    response ~ stage + grade + trt + factor(death),
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, 3), grade = contr.treatment(3, 2),
      trt = contr.treatment(2, 2), "factor(death)" = matrix(c(-3, 2))
    )
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts,
    c(
      NA, "contr.treatment(base=3)", "contr.treatment(base=3)", "contr.treatment(base=3)",
      "contr.treatment(base=2)", "contr.treatment(base=2)", "contr.SAS",
      "custom"
    )
  )
  expect_equal(
    res$contrasts_type,
    c(
      NA, "treatment", "treatment", "treatment", "treatment", "treatment",
      "treatment", "other"
    )
  )

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = "contr.sum", grade = "contr.helmert", trt = "contr.SAS")
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts,
    c(
      NA, "contr.sum", "contr.sum", "contr.sum", "contr.helmert",
      "contr.helmert", "contr.SAS"
    )
  )
  expect_equal(
    res$contrasts_type,
    c(NA, "sum", "sum", "sum", "helmert", "helmert", "treatment")
  )


  skip_if_not_installed("MASS")
  library(MASS)
  mod <- glm(
    response ~ stage + grade + trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.sdif,
      grade = contr.sdif(3),
      trt = "contr.sdif"
    )
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts,
    c(NA, "contr.sdif", "contr.sdif", "contr.sdif", "contr.sdif",
      "contr.sdif", "contr.sdif")
  )
  expect_equal(
    res$contrasts_type,
    c(NA, "sdif", "sdif", "sdif", "sdif", "sdif", "sdif")
  )
})

test_that("test tidy_add_contrasts() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod |> broom::tidy() |> tidy_add_contrasts())

  # could be apply twice (no error)
  expect_error(
    mod |> tidy_and_attach() |> tidy_add_contrasts() |> tidy_add_contrasts(),
    NA
  )
})



test_that("tidy_add_contrasts() works with no intercept models", {
  mod <- glm(response ~ stage + grade - 1, data = gtsummary::trial, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts_type,
    c(
      "no.contrast", "no.contrast", "no.contrast", "no.contrast",
      "treatment", "treatment"
    )
  )
})

test_that("tidy_add_contrasts() works with variables having non standard name", {
  df <- gtsummary::trial |> dplyr::mutate(`grade of kids` = grade)
  mod <- glm(response ~ stage + `grade of kids` + trt, df, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts,
    c(
      NA, "contr.treatment", "contr.treatment", "contr.treatment",
      "contr.treatment", "contr.treatment", "contr.treatment"
    )
  )

  mod <- glm(response ~ stage + `grade of kids` + trt, df,
    family = binomial,
    contrasts = list(`grade of kids` = contr.helmert)
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts,
    c(
      NA, "contr.treatment", "contr.treatment", "contr.treatment",
      "contr.helmert", "contr.helmert", "contr.treatment"
    )
  )
})


test_that("tidy_add_contrasts() works with lme4::lmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  mod <- lme4::lmer(marker ~ stage + grade + (1 | group), df)
  expect_no_error(mod |> tidy_and_attach(tidy_fun = broom.mixed::tidy) |> tidy_add_contrasts())
})


test_that("tidy_add_contrasts() works with lme4::glmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  suppressMessages(
    mod <- lme4::glmer(response ~ stage + grade + (1 | group), df, family = binomial)
  )
  expect_no_error(mod |> tidy_and_attach(tidy_fun = broom.mixed::tidy) |> tidy_add_contrasts())
})


test_that("tidy_add_contrasts() works with survival::coxph", {
  skip_if_not_installed("survival")
  df <- survival::lung |> dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
})

test_that("tidy_add_contrasts() works with survival::survreg", {
  skip_if_not_installed("survival")
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ factor(ecog.ps) + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
})

test_that("tidy_add_contrasts() works with nnet::multinom", {
  skip_if_not_installed("nnet")
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())

  mod <- nnet::multinom(
    grade ~ stage + marker + age,
    data = gtsummary::trial, trace = FALSE,
    contrasts = list(stage = contr.sum)
  )
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_contrasts()
  expect_equal(
    res$contrasts,
    c(
      NA, "contr.sum", "contr.sum", "contr.sum", NA, NA, NA, "contr.sum",
      "contr.sum", "contr.sum", NA, NA
    )
  )
})

test_that("tidy_add_contrasts() works with survey::svyglm", {
  skip_if_not_installed("survey")
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
})

test_that("tidy_add_contrasts() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
})


test_that("tidy_add_contrasts() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
})


test_that("tidy_add_contrasts() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
})


test_that("tidy_add_contrasts() works with geepack::geeglm", {
  skip_if(packageVersion("geepack") < "1.3")

  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
})


test_that("tidy_add_contrasts() works with gam::gam", {
  skip_if_not_installed("gam")
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
})


test_that("tidy_add_contrasts() works with lavaan::lavaan", {
  skip_if_not_installed("lavaan")
  df <- lavaan::HolzingerSwineford1939
  df$grade <- factor(df$grade, ordered = TRUE)
  HS.model <- "visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6 + grade
               speed   =~ x7 + x8 + x9 "
  mod <- lavaan::lavaan(HS.model,
    data = df,
    auto.var = TRUE, auto.fix.first = TRUE,
    auto.cov.lv.x = TRUE
  )
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_contrasts())
})


test_that("model_get_contrasts() works with rstanarm::stan_glm", {
  skip_on_cran()
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("rstanarm")

  mod <- rstanarm::stan_glm(
    response ~ age + grade,
    data = gtsummary::trial,
    refresh = 0,
    family = binomial
  )
  expect_false(
    is.null(mod |> model_get_contrasts())
  )
})
