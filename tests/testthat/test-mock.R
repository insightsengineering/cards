test_that("mock_categorical()", {
  withr::local_options(list(width = 130))
  out <- mock_categorical(
    variables = list(AGEGR1 = factor(c("<65", "65-80", ">80"), levels = c("<65", "65-80", ">80"))),
    by = list(TRTA = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
  )
  expect_s3_class(out, "card")
  expect_named(
    out,
    c(
      "group1", "group1_level", "variable", "variable_level", "context",
      "stat_name", "stat_label", "stat", "fmt_fun", "warning", "error"
    )
  )
  expect_snapshot(out)
})

test_that("mock_categorical() messaging", {
  # incorrect specification of the statistic argument

  expect_error(mock_categorical(
    variables = list(AGEGR1 = factor(c("<65", "65-80", ">80"), levels = c("<65", "65-80", ">80"))),
    statistic = ~ c("NOTASTATISTIC")
  ), "must be vector with one or more of")
  expect_snapshot(
    error = TRUE,
    mock_categorical(
      variables = list(AGEGR1 = factor(c("<65", "65-80", ">80"), levels = c("<65", "65-80", ">80"))),
      statistic = ~ c("NOTASTATISTIC")
    )
  )
})


test_that("mock_continuous()", {
  withr::local_options(list(width = 130))

  out <- mock_continuous(
    variables = c("AGE", "BMIBL")
  )
  expect_s3_class(out, "card")
  expect_named(
    out,
    c(
      "variable", "context", "stat_name", "stat_label", "stat",
      "fmt_fun", "warning", "error"
    )
  )
  expect_snapshot(out)
})

test_that("mock_continuous() messaging", {
  # incorrect specification of the statistic argument
  expect_snapshot(
    error = TRUE,
    mock_continuous(
      variables = c("AGE", "BMIBL"),
      statistic = ~t.test
    )
  )
})

test_that("mock_dichotomous()", {
  withr::local_options(list(width = 130))
  out <- mock_dichotomous(
    variables = list(AGEGR1 = factor("65-80", levels = c("<65", "65-80", ">80"))),
    by = list(TRTA = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
  )
  expect_s3_class(out, "card")
  expect_named(
    out,
    c(
      "group1", "group1_level", "variable", "variable_level", "context",
      "stat_name", "stat_label", "stat", "fmt_fun", "warning", "error"
    )
  )
  expect_snapshot(out)
})

test_that("mock_dichotomous() messaging", {
  # Specifying more than one value to summarize
  expect_snapshot(
    error = TRUE,
    mock_dichotomous(
      variables = list(AGEGR1 = factor(c("<65", "65-80", ">80"), levels = c("<65", "65-80", ">80"))),
      by = list(TRTA = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
    )
  )
})

test_that("mock_missing()", {
  withr::local_options(list(width = 130))
  out <- mock_missing(
    variables = c("AGE", "BMIBL")
  )
  expect_s3_class(out, "card")
  expect_named(
    out,
    c(
      "variable", "context",
      "stat_name", "stat_label", "stat", "fmt_fun", "warning", "error"
    )
  )
  expect_snapshot(out)
})

test_that("mock_missing() messaging", {
  # incorrect specification of the statistic argument
  expect_snapshot(
    error = TRUE,
    mock_missing(
      variables = c("AGE", "BMIBL"),
      statistic = ~letters
    )
  )
})

test_that("mock_attributes()", {
  withr::local_options(list(width = 130))

  out <- mock_attributes(
    label = list(AGE = "Age", BMIBL = "Baseline BMI")
  )
  expect_s3_class(out, "card")
  expect_named(
    out,
    c(
      "variable", "context",
      "stat_name", "stat_label", "stat", "fmt_fun", "warning", "error"
    )
  )
  expect_snapshot(out)
})

test_that("mock_attributes() messaging", {
  # incorrect specification of the label argument
  expect_snapshot(
    error = TRUE,
    mock_attributes(label = c("AGE", "BMIBL"))
  )
})

test_that("mock_total_n()", {
  withr::local_options(list(width = 130))

  out <- mock_total_n()
  expect_s3_class(out, "card")
  expect_named(
    out,
    c(
      "variable", "context",
      "stat_name", "stat_label", "stat", "fmt_fun", "warning", "error"
    )
  )
  expect_snapshot(out)
})
