test_that("tidy_add_reference_rows() works as expected", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.treatment, grade = contr.SAS, trt = contr.sum)
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows()
  if ("stage2" %in% names(coef(mod))) {
    expect_equal(
      res$term,
      c(
        "(Intercept)", "stage1", "stage2", "stage3", "stage4", "grade1",
        "grade2", "grade3", "trt1", "trt2", "grade1:trt1", "grade2:trt1"
      )
    )
  } else {
    expect_equal(
      res$term,
      c(
        "(Intercept)", "stageT1", "stageT2", "stageT3", "stageT4",
        "gradeI", "gradeII", "gradeIII", "trt1", "trt2", "gradeI:trt1",
        "gradeII:trt1"
      )
    )
  }

  expect_equal(
    res$reference_row,
    c(
      NA, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE,
      NA, NA
    )
  )
  expect_equal(
    res$var_class,
    c(
      NA, "factor", "factor", "factor", "factor", "factor", "factor",
      "factor", "character", "character", NA, NA
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    res$var_type,
    c(
      "intercept", "categorical", "categorical", "categorical", "categorical",
      "categorical", "categorical", "categorical", "dichotomous", "dichotomous",
      "interaction", "interaction"
    )
  )
  expect_equal(
    res$var_nlevels,
    c(NA, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 2L, 2L, NA, NA),
    ignore_attr = TRUE
  )

  # no reference row added if other contrasts are used
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.poly, grade = contr.helmert, trt = matrix(c(2, 3)))
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows()
  expect_true(all(is.na(res$reference_row)))

  # no reference row for an interaction only variable
  mod <- lm(age ~ factor(response):marker, gtsummary::trial)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows()
  expect_equal(
    res$reference_row,
    c(NA, NA, NA)
  )

  # no reference row if defined in no_reference_row
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(stage = contr.treatment, grade = contr.SAS, trt = contr.sum)
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows(no_reference_row = c("stage", "grade"))
  if ("stage2" %in% names(coef(mod))) {
    expect_equal(
      res$term,
      c(
        "(Intercept)", "stage2", "stage3", "stage4", "grade1", "grade2",
        "trt1", "trt2", "grade1:trt1", "grade2:trt1"
      )
    )
  } else {
    expect_equal(
      res$term,
      c(
        "(Intercept)", "stageT2", "stageT3", "stageT4", "gradeI", "gradeII",
        "trt1", "trt2", "gradeI:trt1", "gradeII:trt1"
      )
    )
  }



  expect_equal(
    res$reference_row,
    c(NA, NA, NA, NA, NA, NA, FALSE, TRUE, NA, NA)
  )
})

test_that("test tidy_add_reference_rows() checks", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  # expect an error if no model attached
  expect_error(mod |> broom::tidy() |> tidy_add_reference_rows())

  # warning if applied twice
  expect_message(
    mod |>
      tidy_and_attach() |>
      tidy_add_reference_rows() |>
      tidy_add_reference_rows()
  )

  # message if applied after tidy_add_term_labels()
  expect_message(
    mod |>
      tidy_and_attach() |>
      tidy_add_term_labels() |>
      tidy_add_reference_rows()
  )

  # message if applied after tidy_add_n()
  expect_message(
    mod |>
      tidy_and_attach() |>
      tidy_add_n() |>
      tidy_add_reference_rows()
  )

  # error if applied after tidy_add_header_rows()
  expect_error(
    mod |>
      tidy_and_attach() |>
      tidy_add_header_rows() |>
      tidy_add_reference_rows()
  )

  # message or error if non existing variable in no_reference_row
  expect_error(
    mod |> tidy_and_attach() |> tidy_add_reference_rows(no_reference_row = "g")
  )
})

test_that("tidy_add_reference_rows() works with different values of base in contr.treatment()", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, base = 3),
      grade = contr.treatment(3, base = 2),
      trt = contr.treatment(2, base = 2)
    )
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows()
  if ("stage2" %in% names(coef(mod))) {
    expect_equal(
      res$term,
      c(
        "(Intercept)", "stage1", "stage2", "stage3", "stage4", "grade1",
        "grade2", "grade3", "trt1", "trt2", "grade1:trt1", "grade3:trt1"
      )
    )
  } else {
    expect_equal(
      res$term,
      c(
        "(Intercept)", "stageT1", "stageT2", "stageT3", "stageT4", "gradeI",
        "gradeII", "gradeIII", "trt1", "trt2", "gradeI:trt1", "gradeIII:trt1"
      )
    )
  }


  expect_equal(
    res$reference_row,
    c(
      NA, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
      NA, NA
    )
  )
})

test_that("tidy_add_reference_rows() use var_label if available", {
  mod <- glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_variable_labels() |>
    tidy_add_reference_rows()

  expect_equal(
    res$var_label,
    c(
      "(Intercept)", "T Stage", "T Stage", "T Stage", "T Stage",
      "Grade", "Grade", "Grade", "Chemotherapy Treatment", "Chemotherapy Treatment",
      "Grade * Chemotherapy Treatment", "Grade * Chemotherapy Treatment"
    ),
    ignore_attr = TRUE
  )
})

test_that("tidy_add_reference_rows() works with nnet::multinom", {
  skip_if_not_installed("nnet")
  skip_on_cran()
  mod <- nnet::multinom(grade ~ stage + marker + age, data = gtsummary::trial, trace = FALSE)
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows()
  expect_equal(
    res$reference_row,
    c(
      NA, TRUE, FALSE, FALSE, FALSE, NA, NA, NA, TRUE, FALSE, FALSE,
      FALSE, NA, NA
    )
  )
})

test_that("tidy_add_reference_rows() works with lme4::glmer", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("broom.mixed")
  mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial, data = lme4::cbpp
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows()

  expect_equal(
    res[res$reference_row & !is.na(res$reference_row), ]$effect,
    "fixed"
  )
})


test_that("tidy_add_reference_rows() works with glmmTMB::glmmTMB", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("broom.mixed")

  suppressWarnings(
    mod <- glmmTMB::glmmTMB(
      count ~ mined + spp,
      ziformula = ~mined,
      family = poisson,
      data = glmmTMB::Salamanders
    )
  )
  res <- mod |>
    tidy_and_attach() |>
    tidy_add_reference_rows()
  expect_equal(
    res$reference_row,
    c(
      NA, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      NA, TRUE, FALSE
    )
  )
})
