skip_if_not(is_pkg_installed(c("broom.helpers", "withr", "survey", "survival"), reference_pkg = "cardx"))

test_that("construct_model() works", {
  expect_snapshot(
    construct_model(
      data = mtcars |> dplyr::rename(`M P G` = mpg, `c/yl` = cyl),
      formula = reformulate2(c("M P G", "c/yl"), response = "hp"),
      method = "lm"
    ) |>
      ard_regression() |>
      as.data.frame() |>
      dplyr::filter(stat_name %in% c("term", "estimate"))
  )

  expect_equal(
    mtcars[c("mpg", "hp", "vs")] |>
      dplyr::rename(`M P G` = mpg, `h\np` = hp) |>
      names() |>
      bt(),
    c("`M P G`", "`h\np`", "vs")
  )

  expect_equal(
    bt_strip(c("`complex variable name`", "east_variable_name")),
    c("complex variable name", "east_variable_name")
  )

  expect_equal(
    bt("`a b`"),
    "`a b`"
  )

  expect_error(
    check_not_namespaced("geepack::geeglm"),
    "cannot be namespaced"
  )

  # styler: off
  expect_error({
    outside_fun <- function() .as_list_of_exprs(x = !!expr(list()))
    outside_fun()},
    NA
  )
  expect_equal({
      outside_fun <- function() {
        construct_model(
          mtcars,
          formula = mpg ~ cyl,
          method = "lm",
          method.args = !!expr(list())
        ) |>
          coef()
      }
      outside_fun()},
    lm(mpg ~ cyl, mtcars) |> coef()
  )
  # styler: on

  # test function works when passing a function in `method=`
  expect_equal(
    construct_model(
      data = mtcars,
      method = lm,
      formula = mpg ~ cyl + am
    ) |>
      ard_regression(),
    lm(mpg ~ cyl + am, mtcars) |>
      ard_regression()
  )

  # test function works when passing a namespaced function in `method=`
  expect_equal(
    construct_model(
      data = mtcars,
      method = survival::coxph,
      formula = survival::Surv(mpg, am) ~ cyl
    ) |>
      ard_regression(),
    survival::coxph(survival::Surv(mpg, am) ~ cyl, mtcars) |>
      ard_regression()
  )

  # now the survey method -------
  # styler: off
  expect_equal({
    data(api, package = "survey")
    # stratified sample
    survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc) |>
      construct_model(formula = api00 ~ api99, method = "svyglm") |>
      ard_regression() |>
      cards::get_ard_statistics(stat_name %in% "estimate")},
    survey::svyglm(
      api00 ~ api99,
      design = survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
    ) |>
      coef() |>
      getElement(2L) |>
      list(estimate = _)
  )
  # styler: on
})

test_that("construct_model() messaging", {
  expect_snapshot(
    error = TRUE,
    construct_model(
      data = mtcars,
      method = "survival::coxph",
      formula = survival::Surv(mpg, am) ~ cyl
    )
  )

  expect_snapshot(
    error = TRUE,
    construct_model(
      data = mtcars,
      method = letters,
      formula = am ~ cyl
    )
  )

  expect_snapshot(
    error = TRUE,
    construct_model(
      data = mtcars,
      method = "glm",
      formula = am ~ cyl,
      method.args = list(iamnotavalidparameter = "binomial")
    )
  )

  expect_snapshot(
    error = TRUE,
    construct_model(
      data = mtcars,
      method = glm,
      formula = am ~ cyl,
      method.args = list(iamnotavalidparameter = "binomial")
    )
  )

  expect_snapshot(
    error = TRUE,
    {
      data(api, package = "survey")
      design <- survey::svydesign(id = ~1, weights = ~pw, data = apistrat)
      construct_model(
        data = design,
        formula = api00 ~ api99,
        method = "svyglm",
        method.args = list(iamnotavalidparameter = stats::gaussian()),
        package = "survey"
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      data(api, package = "survey")
      design <- survey::svydesign(id = ~1, weights = ~pw, data = apistrat)
      construct_model(
        data = design,
        formula = api00 ~ api99,
        method = "svyglm",
        method.args = list(iamnotavalidparameter = stats::gaussian())
      )
    }
  )
})
