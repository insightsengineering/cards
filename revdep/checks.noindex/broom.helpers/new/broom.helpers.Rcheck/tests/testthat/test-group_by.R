test_that("tidy_group_by() works for basic models", {
  mod <- glm(response ~ stage + grade + trt, gtsummary::trial, family = binomial)
  expect_no_error(
    res <- mod |> tidy_and_attach() |> tidy_group_by()
  )
  expect_false("group_by" %in% colnames(res))

  expect_no_error(
    res <- mod |>
      tidy_and_attach() |>
      tidy_identify_variables() |>
      tidy_group_by(group_by = var_type)
  )
  expect_true("group_by" %in% colnames(res))
  expect_true(is.factor(res$group_by))
  expect_equal(as.character(res$group_by), res$var_type)
})

test_that("tidy_group_by() works with nnet::multinom", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("gtsummary")
  mod <- nnet::multinom(
    grade ~ stage + marker + age,
    data = gtsummary::trial,
    trace = FALSE
  )
  expect_no_error(
    res <- mod |> tidy_and_attach() |> tidy_group_by()
  )
  expect_true("group_by" %in% colnames(res))
  expect_equal(
    levels(res$group_by),
    c("II", "III")
  )
  expect_message(
    res <- mod |>
      tidy_and_attach() |>
      tidy_group_by(group_labels = c(IV = "not found"))
  )
  expect_no_error(
    res <- mod |>
      tidy_and_attach() |>
      tidy_group_by(group_labels = c(III = "group 3"))
  )
  expect_error(
    res <- mod |>
      tidy_and_attach() |>
      tidy_group_by(group_labels = c("group 3"))
  )
  expect_equal(
    levels(res$group_by),
    c("II", "group 3")
  )
  expect_no_error(
    res <- mod |>
      tidy_and_attach() |>
      tidy_identify_variables() |>
      tidy_group_by(group_by = c(var_type, y.level))
  )
  expect_equal(
    length(levels(res$group_by)),
    6
  )
  x <- mod |> tidy_and_attach() |> tidy_identify_variables()
  # by default, keep any pre-existing group_by
  expect_equal(
    x |> tidy_group_by(group_by = "var_type"),
    x |> tidy_group_by(group_by = "var_type") |>  tidy_group_by()
  )
  # NULL to remove any pre-existing group_by
  expect_equal(
    x,
    x |> tidy_group_by() |> tidy_group_by(group_by = NULL)
  )
})
