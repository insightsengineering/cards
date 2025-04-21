test_that("model_get_n() works for basic models", {
  mod <- lm(Sepal.Length ~ ., iris)
  res <- mod |> model_get_n()
  expect_equal(
    res$n_obs,
    c(150, 150, 150, 150, 50, 50, 50),
    ignore_attr = TRUE
  )

  mod <- lm(
    Sepal.Length ~ log(Sepal.Width) + Petal.Length^2,
    iris
  )
  res <- mod |> model_get_n()
  expect_equal(
    res$n_obs,
    c(150, 150, 150),
    ignore_attr = TRUE
  )

  # logistic model
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  res <- mod |> model_get_n()
  expect_equal(
    res$n_obs,
    c(193, 52, 40, 49, 63, 63, 98, 52, 67, 95),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(61, 13, 15, 15, 19, 21, 33, 18, 21, 28),
    ignore_attr = TRUE
  )

  mod <- glm(
    Survived ~ Class * Age + Sex,
    data = Titanic |> as.data.frame(),
    weights = Freq, family = binomial
  )
  res <- mod |> model_get_n()
  expect_equal(
    res$n_obs,
    c(2201, 285, 706, 885, 2092, 470, 261, 627, 885, 325, 109, 1731),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(711, 118, 178, 212, 654, 344, 94, 151, 212, 203, 57, 367),
    ignore_attr = TRUE
  )

  # cbind() syntax
  d <- dplyr::as_tibble(Titanic) |>
    dplyr::group_by(Class, Sex, Age) |>
    dplyr::summarise(
      n_survived = sum(n * (Survived == "Yes")),
      n_dead = sum(n * (Survived == "No"))
    )
  mod <- glm(
    cbind(n_survived, n_dead) ~ Class * Age + Sex,
    data = d,
    family = binomial,
    y = FALSE # should work even if y is not returned
  )
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(
    res$n_obs,
    c(2201, 285, 706, 885, 109, 1731, 24, 79, 0, 325, 2092, 470),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(711, 118, 178, 212, 57, 367, 24, 27, 0, 203, 654, 344),
    ignore_attr = TRUE
  )

  # Poisson without offset
  mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = poisson)
  res <- mod |> model_get_n()
  expect_equal(
    res$n_obs,
    c(183, 183, 58, 60, 94, 29, 33, 65, 89),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(58, 58, 17, 20, 31, 10, 8, 21, 27),
    ignore_attr = TRUE
  )
  expect_equal(
    res$exposure,
    c(183, 183, 58, 60, 94, 29, 33, 65, 89),
    ignore_attr = TRUE
  )

  # Poisson with offset
  mod <- glm(
    response ~ trt * grade + offset(log(ttdeath)),
    gtsummary::trial,
    family = poisson,
    weights = rep_len(1:2, 200)
  )
  res <- mod |> model_get_n()
  expect_equal(
    res$n_obs,
    c(292, 151, 94, 92, 49, 49, 141, 106),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(96, 53, 28, 31, 19, 12, 43, 37),
    ignore_attr = TRUE
  )
  expect_equal(
    res$exposure |> round(),
    c(5819, 2914, 1826, 1766, 887, 916, 2905, 2227),
    ignore_attr = TRUE
  )

  # interaction only terms
  mod <- glm(
    Survived ~ Class:Age,
    data = Titanic |> as.data.frame(),
    weights = Freq, family = binomial
  )
  res <- mod |> model_get_n()
  expect_equal(
    res$n_obs,
    c(2201, 6, 24, 79, 0, 319, 261, 627, 885),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(711, 6, 24, 27, 0, 197, 94, 151, 212),
    ignore_attr = TRUE
  )
})


test_that("model_get_n() handles variables having non standard name", {
  df <- gtsummary::trial |> dplyr::mutate(`grade of kids` = grade)
  mod <- glm(response ~ stage + `grade of kids` + trt, df,
    family = binomial,
    contrasts = list(`grade of kids` = contr.sum)
  )
  expect_no_error(
    res <- mod |> model_get_n()
  )
})


test_that("model_get_n() works with different contrasts", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.treatment, grade = contr.SAS, trt = contr.SAS)
  )
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs", "n_event"))
  if ("stage2" %in% names(coef(mod))) {
    expect_equal(
      res$term,
      c(
        "(Intercept)", "stage2", "stage3", "stage4", "grade1", "grade2",
        "trt1", "grade1:trt1", "grade2:trt1", "stage1", "grade3", "trt2"
      )
    )
  } else {
    expect_equal(
      res$term,
      c(
        "(Intercept)", "stageT2", "stageT3", "stageT4", "gradeI", "gradeII",
        "trtDrug A", "gradeI:trtDrug A", "gradeII:trtDrug A", "stageT1",
        "gradeIII", "trtDrug B"
      )
    )
  }
  expect_equal(
    res$n_obs,
    c(193, 52, 40, 49, 67, 63, 95, 35, 30, 52, 63, 98),
    ignore_attr = TRUE
  )

  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.poly, grade = contr.helmert, trt = contr.sum)
  )
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs", "n_event"))
  expect_equal(
    res$term,
    c(
      "(Intercept)", "stage.L", "stage.Q", "stage.C", "grade1", "grade2",
      "trt1", "grade1:trt1", "grade2:trt1", "trt2"
    )
  )
  expect_equal(
    res$n_obs,
    c(193, 193, 193, 193, 63, 63, 95, 62, 95, 98),
    ignore_attr = TRUE
  )
})


test_that("model_get_n() works with stats::poly()", {
  skip_on_cran()
  mod <- lm(Sepal.Length ~ poly(Sepal.Width, 3) + poly(Petal.Length, 2), iris)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs"))
  expect_equal(
    res$term,
    c(
      "(Intercept)", "poly(Sepal.Width, 3)1", "poly(Sepal.Width, 3)2",
      "poly(Sepal.Width, 3)3", "poly(Petal.Length, 2)1",
      "poly(Petal.Length, 2)2"
    )
  )
  expect_equal(
    res$n_obs,
    c(150, 150, 150, 150, 150, 150),
    ignore_attr = TRUE
  )
})


test_that("model_get_n() works with lme4::lmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  mod <- lme4::lmer(marker ~ stage + grade + (1 | group), df)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs"))
})


test_that("model_get_n() works with lme4::glmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  df <- gtsummary::trial
  df$stage <- as.character(df$stage)
  df$group <- rep.int(1:2, 100)
  df$response <- factor(df$response)
  suppressMessages(
    mod <- lme4::glmer(response ~ stage + grade + (1 | group), df, family = binomial)
  )
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs", "n_event"))
})


test_that("model_get_n() works with survival::coxph", {
  skip_on_cran()
  skip_if_not_installed("survival")
  df <- survival::lung |> dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(
    survival::Surv(time, status) ~ ph.ecog + age + sex,
    data = df
  )
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(
    names(res),
    c("term", "n_obs", "n_ind", "n_event", "exposure")
  )

  test <- list(
    start = c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
    stop = c(2, 3, 6, 7, 8, 9, 9, 9, 14, 17),
    event = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
    x = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0)
  )
  mod <- survival::coxph(survival::Surv(start, stop, event) ~ x, test)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(
    names(res),
    c("term", "n_obs", "n_ind", "n_event", "exposure")
  )
  expect_equal(res$n_obs, c(10, 10), ignore_attr = TRUE)
  expect_equal(res$n_ind, c(10, 10), ignore_attr = TRUE)
  expect_equal(res$n_event, c(7, 7), ignore_attr = TRUE)
  expect_equal(res$exposure, c(43, 43), ignore_attr = TRUE)

  # specific case when missing values in the `id`
  # should not result in a warning
  mod <- survival::coxph(
    survival::Surv(ttdeath, death) ~ age + grade,
    id = response,
    data = gtsummary::trial
  )
  expect_no_warning(mod |> model_get_n())
})

test_that("model_get_n() works with survival::survreg", {
  skip_on_cran()
  skip_if_not_installed("survival")
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ factor(ecog.ps) + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(
    names(res),
    c("term", "n_obs", "n_ind", "n_event", "exposure")
  )
})

test_that("model_get_n() works with nnet::multinom", {
  skip_if_not_installed("nnet")
  skip_on_cran()
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("y.level", "term", "n_obs", "n_event"))
  expect_equal(
    res$y.level,
    c(
      "II", "II", "II", "II", "II", "II", "II", "III", "III", "III",
      "III", "III", "III", "III"
    )
  )
  expect_equal(
    res$n_obs,
    c(179, 52, 37, 43, 179, 179, 47, 179, 52, 37, 43, 179, 179, 47),
    ignore_attr = TRUE
  )
  expect_equal(
    res$n_event,
    c(57, 16, 8, 12, 57, 57, 21, 58, 18, 12, 16, 58, 58, 12),
    ignore_attr = TRUE
  )

  # when y is not coded as a factor
  mod <- nnet::multinom(race ~ age + lwt + bwt, data = MASS::birthwt, trace = FALSE)
  expect_true(mod |> model_get_n() |> nrow() > 0)
})

test_that("model_get_n() works with survey::svyglm", {
  skip_on_cran()
  skip_if_not_installed("survey")
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs", "n_event"))

  mod <- survey::svyglm(response ~ age + grade + offset(log(ttdeath)), df, family = quasipoisson)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs", "n_event", "exposure"))

  df <- survey::svydesign(
    ~1,
    weights = ~Freq,
    data = as.data.frame(Titanic) |> dplyr::filter(Freq > 0)
  )
  mod <- survey::svyglm(Survived ~ Class + Age * Sex, df, family = quasibinomial)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs", "n_event"))
  expect_equal(
    res$n_obs,
    c(2201, 285, 706, 885, 2092, 470, 425, 325, 109, 1731),
    ignore_attr = TRUE
  )
})

test_that("model_get_n() works with ordinal::clm", {
  skip_on_cran()
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs"))
  # note: no nevent computed for ordinal models
})


test_that("model_get_n() works with ordinal::clmm", {
  skip_on_cran()
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs"))
})


test_that("model_get_n() works with MASS::polr", {
  skip_on_cran()
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs"))
})


test_that("model_get_n() works with geepack::geeglm", {
  skip_on_cran()
  skip_if(packageVersion("geepack") < "1.3")

  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs"))

  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson(), corstr = "ar1")
  )
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs", "n_event", "exposure"))
})


test_that("model_get_n() works with gam::gam", {
  skip_on_cran()
  skip_if_not_installed("gam")
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_no_error(res <- mod |> model_get_n())
  expect_equal(names(res), c("term", "n_obs", "n_event"))
})


test_that("model_get_n() works with lavaan::lavaan", {
  skip_on_cran()
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
  expect_no_error(res <- mod |> model_get_n())
  expect_null(res)
  expect_null(mod |> model_get_response())
  expect_null(mod |> model_get_weights())
  expect_null(mod |> model_get_offset())
  expect_null(mod |> model_compute_terms_contributions())
})

test_that("model_get_n() works with tidycmprsk::crr", {
  skip_on_cran()
  skip_if_not_installed("tidycmprsk")
  skip_if_not_installed("survival")

  mod <- tidycmprsk::crr(
    survival::Surv(ttdeath, death_cr) ~ age + grade,
    tidycmprsk::trial
  )
  res <- mod |> tidy_plus_plus()
  expect_equal(
    res$n_event,
    c(52, 16, 15, 21),
    ignore_attr = TRUE
  )
})

test_that("tidy_add_n() does not duplicates rows with gam model", {
  skip_on_cran()
  skip_if_not_installed("mgcv")
  skip_if_not_installed("gtsummary")

  mod <- mgcv::gam(
    marker ~ s(age, bs = "ad", k = -1) + grade + ti(age, by = grade, bs = "fs"),
    data = gtsummary::trial,
    method = "REML",
    family = gaussian
  )

  res <- mod |>
    tidy_and_attach(tidy_fun = gtsummary::tidy_gam) |>
    tidy_add_n()
  expect_equal(nrow(res), 7L)
})
