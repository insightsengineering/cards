test_that("tidy_add_n() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_n()
  expect_equal(
    res$n_obs,
    c(193, 52, 40, 49, 63, 63, 98),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(61, 13, 15, 15, 19, 21, 33),
    ignore_attr = TRUE
  )
  expect_equal(attr(res, "N_obs"), 193)
  expect_equal(attr(res, "N_event"), 61)

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.sum, grade = contr.helmert, trt = contr.SAS)
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_n()
  expect_equal(
    res$n_obs,
    c(193, 52, 52, 40, 63, 63, 95),
    ignore_attr = TRUE
  )
  expect_equal(attr(res, "N_obs"), 193)
  expect_equal(attr(res, "N_event"), 61)

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.poly, grade = contr.treatment, trt = matrix(c(-3, 2)))
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_n()
  expect_equal(
    res$n_obs,
    c(193, 193, 193, 193, 63, 63, 98),
    ignore_attr = TRUE
  )
  expect_equal(attr(res, "N_obs"), 193)
  expect_equal(attr(res, "N_event"), 61)

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
    tidy_add_n()
  expect_equal(
    res$n_obs,
    c(193, 52, 52, 49, 67, 63, 95, 107),
    ignore_attr = TRUE
  )
  expect_equal(attr(res, "N_obs"), 193)
  expect_equal(attr(res, "N_event"), 61)

  mod <- glm(response ~ stage + grade + trt, gtsummary::trial,
    family = binomial,
    contrasts = list(stage = "contr.sum", grade = "contr.helmert", trt = "contr.SAS")
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_n()
  expect_equal(
    res$n_obs,
    c(193, 52, 52, 40, 63, 63, 95),
    ignore_attr = TRUE
  )


  mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = poisson)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_n()
  expect_equal(
    res$n_obs,
    c(183, 183, 58, 60, 94, 29, 33),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(58, 58, 17, 20, 31, 10, 8),
    ignore_attr = TRUE
  )
  expect_equal(
    res$exposure,
    c(183, 183, 58, 60, 94, 29, 33),
    ignore_attr = TRUE
  )
  expect_equal(attr(res, "N_obs"), 183)
  expect_equal(attr(res, "N_event"), 58)
  expect_equal(attr(res, "Exposure"), 183)

  mod <- glm(
    response ~ trt * grade + offset(log(ttdeath)),
    gtsummary::trial,
    family = poisson,
    weights = rep_len(1:2, 200)
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_n()
  expect_equal(
    res$n_obs,
    c(292, 151, 94, 92, 49, 49),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(96, 53, 28, 31, 19, 12),
    ignore_attr = TRUE
  )
  expect_equal(
    res$exposure,
    c(5819.07, 2913.6, 1826.26, 1765.52, 887.22, 915.56),
    ignore_attr = TRUE
  )
  expect_equal(attr(res, "N_obs"), 292)
  expect_equal(attr(res, "N_event"), 96)
  expect_equal(attr(res, "Exposure"), 5819.07)
})

test_that("test tidy_add_n() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod |> broom::tidy() |> tidy_add_n())

  # could be apply twice (no error)
  expect_no_error(
    mod |> tidy_and_attach() |> tidy_add_n() |> tidy_add_n()
  )
})


test_that("tidy_add_n() works with variables having non standard name", {
  df <- gtsummary::trial |> dplyr::mutate(`grade of kids` = grade)
  mod <- glm(response ~ stage + `grade of kids` + trt, df, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_n()
  expect_equal(
    res$n_obs,
    c(193, 52, 40, 49, 63, 63, 98),
    ignore_attr = TRUE
  )
})


test_that("tidy_add_n() works with lme4::lmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  mod <- lme4::lmer(marker ~ stage + grade + (1 | group), df)
  expect_no_error(mod |> tidy_and_attach(tidy_fun = broom.mixed::tidy) |> tidy_add_n())
})


test_that("tidy_add_n() works with lme4::glmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  suppressMessages(
    mod <- lme4::glmer(response ~ stage + grade + (1 | group), df, family = binomial)
  )
  expect_no_error(mod |> tidy_and_attach(tidy_fun = broom.mixed::tidy) |> tidy_add_n())
})


test_that("tidy_add_n() works with survival::coxph", {
  skip_on_cran()
  skip_if_not_installed("survival")
  df <- survival::lung |> dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_no_error(res <- mod |> tidy_and_attach() |> tidy_add_n())
  expect_equal(res$n_ind, c(227, 227, 90), ignore_attr = TRUE)
  expect_equal(attr(res, "N_ind"), 227)
})

test_that("tidy_add_n() works with survival::survreg", {
  skip_on_cran()
  skip_if_not_installed("survival")
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ factor(ecog.ps) + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_n())
})

test_that("tidy_add_n() works with nnet::multinom", {
  skip_if_not_installed("nnet")
  skip_on_cran()
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_n())

  mod <- nnet::multinom(
    grade ~ stage + marker + age,
    data = gtsummary::trial, trace = FALSE,
    contrasts = list(stage = contr.sum)
  )
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_n())
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_n()
  expect_equal(
    res$n_obs,
    c(179, 47, 52, 37, 179, 179, 179, 47, 52, 37, 179, 179),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(57, 21, 16, 8, 57, 57, 58, 12, 18, 12, 58, 58),
    ignore_attr = TRUE
  )

  # when y is not coded as a factor
  mod <- nnet::multinom(race ~ age + lwt + bwt, data = MASS::birthwt, trace = FALSE)
  expect_no_error(
    mod |> tidy_and_attach() |> tidy_add_n()
  )
})

test_that("tidy_add_n() works with survey::svyglm", {
  skip_if_not_installed("survey")
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_n())
})

test_that("tidy_add_n() works with ordinal::clm", {
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_n())
})


test_that("tidy_add_n() works with ordinal::clmm", {
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_n())
})


test_that("tidy_add_n() works with MASS::polr", {
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_n())
})


test_that("tidy_add_n() works with geepack::geeglm", {
  skip_if(packageVersion("geepack") < "1.3")

  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_n())
})


test_that("tidy_add_n() works with gam::gam", {
  skip_if_not_installed("gam")
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_n())
})


test_that("tidy_add_n() works with lavaan::lavaan", {
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
  expect_no_error(res <- mod |> tidy_and_attach() |> tidy_add_n())
  expect_true(all(is.na(res$n)))
})

test_that("model_compute_terms_contributions() with subset", {
  mod <- glm(mpg ~ gear, data = mtcars, subset = mpg < 30)
  expect_no_warning(
    res <- mod |> model_compute_terms_contributions()
  )
  expect_equal(
    nrow(res),
    nrow(mtcars[mtcars$mpg < 30, ])
  )
})
