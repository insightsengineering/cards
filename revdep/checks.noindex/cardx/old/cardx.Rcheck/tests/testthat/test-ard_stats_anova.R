skip_if_not(is_pkg_installed(c("broom", "withr", "lme4", "survival", "geepack"), reference_pkg = "cardx"))

test_that("ard_stats_anova.anova() works", {
  expect_equal(
    anova(
      lm(mpg ~ am, mtcars),
      lm(mpg ~ am + hp, mtcars)
    ) |>
      ard_stats_anova() |>
      dplyr::select(variable, stat_name, stat) |>
      dplyr::filter(!stat_name %in% "method"),
    anova(
      lm(mpg ~ am, mtcars),
      lm(mpg ~ am + hp, mtcars)
    ) |>
      broom::tidy() |>
      dplyr::mutate(
        across(everything(), as.list),
        variable = paste0("model_", dplyr::row_number())
      ) |>
      tidyr::pivot_longer(
        cols = -variable,
        names_to = "stat_name",
        values_to = "stat"
      ) |>
      dplyr::filter(!is.na(stat)),
    ignore_attr = TRUE
  )
})

test_that("ard_stats_anova.data.frame() works", {
  expect_equal(
    anova(
      lm(mpg ~ am, mtcars),
      lm(mpg ~ am + hp, mtcars)
    ) |>
      ard_stats_anova(),
    ard_stats_anova(
      x = mtcars,
      formulas = list(mpg ~ am, mpg ~ am + hp),
      method = "lm"
    )
  )

  # function works with a non-standard evaluation argument
  expect_error(
    ard_anova_geeglm <-
      ard_stats_anova(
        x = mtcars,
        formulas = list(mpg ~ hp, mpg ~ hp + vs),
        method = "geeglm",
        method.args = list(id = cyl),
        package = "geepack"
      ),
    NA
  )
  expect_equal(
    ard_anova_geeglm |>
      dplyr::filter(stat_name == "p.value") |>
      dplyr::pull(stat) |>
      unlist(),
    suppressWarnings(
      anova(
        geepack::geeglm(mpg ~ hp, data = mtcars, id = cyl),
        geepack::geeglm(mpg ~ hp + vs, data = mtcars, id = cyl)
      ) |>
        broom::tidy() |>
        dplyr::pull(p.value)
    )
  )

  # function works with a non-base R package
  expect_error(
    ard_anova_glmer <-
      ard_stats_anova(
        x = mtcars,
        formulas = list(am ~ 1 + (1 | vs), am ~ mpg + (1 | vs)),
        method = "glmer",
        method.args = list(family = binomial),
        package = "lme4"
      ),
    NA
  )
  expect_equal(
    ard_anova_glmer |>
      dplyr::filter(stat_name == "p.value") |>
      dplyr::pull(stat) |>
      unlist(),
    suppressMessages(
      anova(
        lme4::glmer(am ~ 1 + (1 | vs), data = mtcars, family = binomial),
        lme4::glmer(am ~ mpg + (1 | vs), data = mtcars, family = binomial)
      )
    ) |>
      broom::tidy() |>
      dplyr::pull(p.value) |>
      keep(~ !is.na(.))
  )

  # adding a testing with more complex env handling
  args_fun <- function(args) {
    ard_stats_anova(
      x = mtcars,
      formulas = list(am ~ 1 + (1 | vs), am ~ mpg + (1 | vs)),
      method = "glmer",
      method.args = {{ args }},
      package = "lme4"
    )
  }
  ard_anova_glmer2 <- args_fun(list(family = binomial))
  expect_equal(
    ard_anova_glmer2,
    ard_anova_glmer
  )

  # adding a testing with more complex env handling with NSE
  args_fun <- function(args) {
    ard_stats_anova(
      x = mtcars,
      formulas = list(mpg ~ hp, mpg ~ hp + vs),
      method = "geeglm",
      method.args = {{ args }},
      package = "geepack"
    )
  }
  ard_anova_geeglm2 <- args_fun(list(id = cyl))
  expect_equal(
    ard_anova_geeglm2$stat,
    ard_anova_geeglm$stat
  )
})

test_that("ard_stats_anova.data.frame() error messaging", {
  expect_true(
    ard_stats_anova(
      x = mtcars,
      formulas = list(mpg ~ am, mpg ~ am + hp),
      method = "base::lm"
    ) |>
      dplyr::pull("error") |>
      unique() |>
      unlist() |>
      grepl(pattern = "^Argument `method` cannot be namespaced*", x = _)
  )
})
