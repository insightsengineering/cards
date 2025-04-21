test_that("tidy_add_term_labels() works for basic models", {
  mod <- lm(Petal.Length ~ Petal.Width, iris)
  expect_no_error(
    mod |> tidy_and_attach() |> tidy_add_term_labels()
  )

  df <- gtsummary::trial
  mod <- glm(response ~ age + grade + trt, df, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_term_labels()
  expect_equal(
    res$label,
    c("(Intercept)", "Age", "II", "III", "Drug B"),
    ignore_attr = TRUE
  )

  df <- gtsummary::trial
  mod <- glm(response ~ age + grade + trt, df, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows() |>
    tidy_add_term_labels()
  expect_equal(
    res$label,
    c("(Intercept)", "Age", "I", "II", "III", "Drug A", "Drug B"),
    ignore_attr = TRUE
  )

  # if labels provided in `labels`, taken into account
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows() |>
    tidy_add_term_labels(
      labels = list(
        "(Intercept)" = "the intercept",
        "trtDrug A" = "the reference term",
        gradeIII = "third grade"
      )
    )
  expect_equal(
    res$label,
    c(
      "the intercept", "Age", "I", "II", "third grade", "the reference term",
      "Drug B"
    ),
    ignore_attr = TRUE
  )
  # no error if providing labels not corresponding to an existing variable
  # but display a message
  expect_no_error(
    mod |>
      tidy_and_attach() |>
      tidy_add_term_labels(
        labels = list(aaa = "aaa", bbb = "bbb", ccc = 44)
      )
  )
  expect_message(
    mod |>
      tidy_and_attach() |>
      tidy_add_term_labels(
        labels = list(aaa = "aaa", bbb = "bbb", ccc = 44)
      )
  )

  expect_error(
    mod |>
      tidy_and_attach() |>
      tidy_add_term_labels(
        labels = list(aaa = "aaa", bbb = "bbb", ccc = 44),
        strict = TRUE
      )
  )

  # model with an interaction term only
  mod <- lm(age ~ factor(response):marker, gtsummary::trial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_term_labels()
  expect_equal(
    res$label,
    c("(Intercept)", "0 * Marker Level (ng/mL)", "1 * Marker Level (ng/mL)"),
    ignore_attr = TRUE
  )
})


test_that("test tidy_add_term_labels() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod |> broom::tidy() |> tidy_add_term_labels())

  # could be apply twice (no error)
  expect_no_error(
    mod |> tidy_and_attach() |> tidy_add_term_labels() |> tidy_add_term_labels()
  )

  # cannot be applied after tidy_add_header_rows
  expect_error(
    mod |> tidy_and_attach() |> tidy_add_header_rows() |> tidy_add_term_labels()
  )
})

test_that("tidy_add_term_labels() correctly manages interaction terms", {
  df <- gtsummary::trial
  mod <- glm(response ~ age * grade * trt, df, family = binomial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows() |>
    tidy_add_term_labels()
  expect_equal(
    res$label,
    c(
      "(Intercept)", "Age", "I", "II", "III", "Drug A", "Drug B",
      "Age * II", "Age * III", "Age * Drug B", "II * Drug B", "III * Drug B",
      "Age * II * Drug B", "Age * III * Drug B"
    ),
    ignore_attr = TRUE
  )

  # custom separator and custom labels for certain interaction terms
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows() |>
    tidy_add_term_labels(
      interaction_sep = ":::",
      labels = c(
        "age:gradeII" = "custom interaction label",
        "gradeII:trtDrug B" = "a second custom label"
      )
    )
  expect_equal(
    res$label,
    c(
      "(Intercept)", "Age", "I", "II", "III", "Drug A", "Drug B",
      "custom interaction label", "Age:::III", "Age:::Drug B", "a second custom label",
      "III:::Drug B", "Age:::II:::Drug B", "Age:::III:::Drug B"
    ),
    ignore_attr = TRUE
  )

  # case with sum contrasts
  mod <- lm(
    marker ~ stage:ttdeath + stage,
    data = gtsummary::trial,
    contrasts = list(stage = "contr.sum")
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows() |>
    tidy_add_term_labels()
  expect_equal(
    res$label,
    c(
      "(Intercept)", "T1", "T2", "T3", "T4", "T1 * Months to Death/Censor",
      "T2 * Months to Death/Censor", "T3 * Months to Death/Censor",
      "T4 * Months to Death/Censor"
    ),
    ignore_attr = TRUE
  )

  # complex case: model with no intercept and sum contrasts
  mod <- lm(
    Petal.Length ~ Species * Petal.Width - 1,
    data = iris,
    contrasts = list(Species = contr.sum)
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows() |>
    tidy_add_term_labels()
  expect_equal(
    res$label,
    c(
      "setosa", "versicolor", "virginica", "Petal.Width",
      "setosa * Petal.Width", "versicolor * Petal.Width"
    ),
    ignore_attr = TRUE
  )
})

test_that("tidy_add_term_labels() works with poly or helmert contrasts", {
  mod <- glm(
    response ~ stage + grade + trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.sum, grade = contr.helmert, trt = contr.SAS)
  )
  # should not produce an error
  expect_no_error(
    mod |> tidy_and_attach() |> tidy_add_term_labels()
  )
})

test_that("tidy_add_term_labels() works with sdif contrasts", {
  skip_if_not_installed("MASS")
  mod <- glm(
    response ~ stage + grade,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = MASS::contr.sdif, grade = MASS::contr.sdif)
  )
  # should not produce an error
  expect_no_error(
    res <- mod |> tidy_and_attach() |> tidy_add_term_labels()
  )
  expect_equal(
    res$label,
    c(
      `(Intercept)` = "(Intercept)", `stageT2-T1` = "T2 - T1",
      `stageT3-T2` = "T3 - T2", `stageT4-T3` = "T4 - T3",
      `gradeII-I` = "II - I", `gradeIII-II` = "III - II"
    ),
    ignore_attr = TRUE
  )
  # should not produce an error
  expect_no_error(
    res <- mod |>
      tidy_and_attach(exponentiate = TRUE) |>
      tidy_add_term_labels()
  )
  expect_equal(
    res$label,
    c(
      `(Intercept)` = "(Intercept)", `stageT2-T1` = "T2 / T1",
      `stageT3-T2` = "T3 / T2", `stageT4-T3` = "T4 / T3",
      `gradeII-I` = "II / I", `gradeIII-II` = "III / II"
    ),
    ignore_attr = TRUE
  )
})


test_that("tidy_add_term_labels() works with variables having non standard name", {
  skip_on_cran()
  df <- gtsummary::trial |> dplyr::rename(
    `grade of kids...` = grade,
    `?? treatment ++ response ...` = response
  )
  mod <- lm(age ~ marker * `grade of kids...` + factor(`?? treatment ++ response ...`), df)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows() |>
    tidy_add_term_labels()
  expect_equal(
    res$label,
    c(
      "(Intercept)", "Marker Level (ng/mL)", "I", "II", "III", "0",
      "1", "Marker Level (ng/mL) * II", "Marker Level (ng/mL) * III"
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    res$variable,
    c(
      "(Intercept)", "marker", "grade of kids...", "grade of kids...", "grade of kids...",
      "factor(`?? treatment ++ response ...`)", "factor(`?? treatment ++ response ...`)",
      "marker:grade of kids...", "marker:grade of kids..."
    )
  )

  res <-
    lm(
      response ~ `age at dx` + `drug type`,
      data = gtsummary::trial |>
        dplyr::select(response, `age at dx` = age, `drug type` = trt)
    ) |>
    tidy_and_attach() |>
    tidy_add_variable_labels(list(`age at dx` = "AGGGGGGGE")) |>
    tidy_add_term_labels()
  expect_equal(
    res$label,
    c("(Intercept)", "AGGGGGGGE", "Drug B"),
    ignore_attr = TRUE
  )
})


test_that("tidy_add_term_labels() works with stats::poly()", {
  skip_on_cran()
  df <- iris |> labelled::set_variable_labels(Petal.Length = "Length of petal")
  mod <- lm(Sepal.Length ~ poly(Sepal.Width, 3) + poly(Petal.Length, 2), df)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_term_labels()
  expect_equal(
    res$label,
    c(
      "(Intercept)", "Sepal.Width", "Sepal.Width²", "Sepal.Width³",
      "Petal.Length", "Petal.Length²"
    ),
    ignore_attr = TRUE
  )
})

skip_on_cran()

test_that("tidy_add_term_labels() works with lme4::lmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  expect_no_error(mod |> tidy_and_attach(tidy_fun = broom.mixed::tidy) |> tidy_add_term_labels())
})


test_that("tidy_add_term_labels() works with lme4::glmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial, data = lme4::cbpp
  )
  expect_no_error(mod |> tidy_and_attach(tidy_fun = broom.mixed::tidy) |> tidy_add_term_labels())
})


test_that("tidy_add_term_labels() works with survival::coxph", {
  skip_on_cran()
  skip_if_not_installed("survival")
  df <- survival::lung |> dplyr::mutate(sex = factor(sex))
  mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})

test_that("tidy_add_term_labels() works with survival::survreg", {
  skip_on_cran()
  skip_if_not_installed("survival")
  mod <- survival::survreg(
    survival::Surv(futime, fustat) ~ ecog.ps + rx,
    survival::ovarian,
    dist = "exponential"
  )
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})

test_that("tidy_add_term_labels() works with nnet::multinom", {
  skip_on_cran()
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})

test_that("tidy_add_term_labels() works with survey::svyglm", {
  skip_on_cran()
  skip_if_not_installed("survey")
  df <- survey::svydesign(~1, weights = ~1, data = gtsummary::trial)
  mod <- survey::svyglm(response ~ age + grade * trt, df, family = quasibinomial)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})

test_that("tidy_add_term_labels() works with ordinal::clm", {
  skip_on_cran()
  mod <- ordinal::clm(rating ~ temp * contact, data = ordinal::wine)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})


test_that("tidy_add_term_labels() works with ordinal::clmm", {
  skip_on_cran()
  mod <- ordinal::clmm(rating ~ temp * contact + (1 | judge), data = ordinal::wine)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})


test_that("tidy_add_term_labels() works with MASS::polr", {
  skip_on_cran()
  mod <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})


test_that("tidy_add_term_labels() works with geepack::geeglm", {
  skip_on_cran()
  skip_if(packageVersion("geepack") < "1.3")

  df <- geepack::dietox
  df$Cu <- as.factor(df$Cu)
  mf <- formula(Weight ~ Cu * Time)
  suppressWarnings(
    mod <- geepack::geeglm(mf, data = df, id = Pig, family = poisson("identity"), corstr = "ar1")
  )
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})


test_that("tidy_add_term_labels() works with gam::gam", {
  skip_on_cran()
  skip_if_not_installed("gam")
  data(kyphosis, package = "gam")
  mod <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())

  mod <- suppressWarnings(gam::gam(
    Ozone^(1 / 3) ~ gam::lo(Solar.R) + gam::lo(Wind, Temp),
    data = datasets::airquality, na = gam::na.gam.replace
  ))
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})


test_that("tidy_add_term_labels() works with lavaan::lavaan", {
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
  expect_no_error(mod |> tidy_and_attach() |> tidy_add_term_labels())
})
