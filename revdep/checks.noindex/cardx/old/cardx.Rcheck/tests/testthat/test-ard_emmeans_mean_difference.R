skip_if_not(is_pkg_installed(c("emmeans", "survey", "lme4")))

test_that("ard_emmeans_mean_difference() works", {
  expect_error(
    ard_emmeans_mean_difference <-
      ard_emmeans_mean_difference(
        data = mtcars,
        formula = vs ~ am + mpg,
        method = "glm",
        method.args = list(family = binomial),
        response_type = "dichotomous"
      ),
    NA
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_difference, stat_name %in% "method"),
    list(method = "Least-squares adjusted mean difference")
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_difference, stat_name %in% "estimate") |>
      unlist() |>
      unname(),
    glm(vs ~ am + mpg, data = mtcars, family = binomial) |>
      emmeans::emmeans(specs = ~am, regrid = "response") |>
      emmeans::contrast(method = "pairwise") |>
      summary(infer = TRUE) |>
      getElement("estimate")
  )


  expect_error(
    ard_emmeans_mean_difference_lme4 <-
      ard_emmeans_mean_difference(
        data = mtcars,
        formula = vs ~ am + (1 | cyl),
        method = "glmer",
        method.args = list(family = binomial),
        package = "lme4",
        response_type = "dichotomous"
      ),
    NA
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_difference_lme4, stat_name %in% "method"),
    list(method = "Least-squares mean difference")
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_difference_lme4, stat_name %in% "estimate") |>
      unlist() |>
      unname(),
    lme4::glmer(vs ~ am + (1 | cyl), data = mtcars, family = binomial) |>
      emmeans::emmeans(specs = ~am, regrid = "response") |>
      emmeans::contrast(method = "pairwise") |>
      summary(infer = TRUE) |>
      getElement("estimate")
  )


  #styler: off
  expect_error({
    data(api, package = "survey")
    ard_emmeans_mean_difference_svy <-
      survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
      ard_emmeans_mean_difference(
        formula = api00 ~ sch.wide,
        method = "svyglm",
        package = "survey"
      )},
    NA
  )
  # styler: on
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_difference_svy, stat_name %in% "method"),
    list(method = "Least-squares mean difference")
  )
  expect_equal(
    cards::get_ard_statistics(ard_emmeans_mean_difference_svy, stat_name %in% "estimate") |>
      unlist() |>
      unname(),
    survey::svyglm(api00 ~ sch.wide, design = survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)) |>
      emmeans::emmeans(specs = ~sch.wide, regrid = "response") |>
      emmeans::contrast(method = "pairwise") |>
      summary(infer = TRUE) |>
      getElement("estimate")
  )
})

test_that("ard_emmeans_mean_difference() follows ard structure", {
  expect_silent(
    ard_emmeans_mean_difference(
      data = mtcars,
      formula = vs ~ am + mpg,
      method = "glm",
      method.args = list(family = binomial),
      response_type = "dichotomous"
    ) |>
      cards::check_ard_structure()
  )
})
