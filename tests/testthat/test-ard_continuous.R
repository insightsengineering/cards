test_that("ard_continuous() works", {
  expect_error(
    ard_test <-
      ard_continuous(mtcars, variables = c(mpg, hp), by = c(am, vs)),
    NA
  )

  expect_equal(
    get_ard_statistics(
      ard_test,
      group1_level %in% 0,
      group2_level %in% 0,
      variable %in% "mpg",
      stat_name %in% c("N", "mean")
    ),
    list(N = with(mtcars, length(mpg[am %in% 0 & vs %in% 0])),
         mean = with(mtcars, mean(mpg[am %in% 0 & vs %in% 0]))),
    ignore_attr = TRUE
  )

  expect_equal(
    ard_continuous(
      mtcars,
      variables = starts_with("xxxxx")
    ),
    dplyr::tibble()
  )
})


test_that("ard_continuous(fmt_fn) argument works", {
  ard_continuous(
    ADSL,
    variables = "AGE",
    statistics = list(AGE = continuous_variable_summary_fns(c("N", "mean", "median"))),
    fmt_fn =
      list(
        AGE =
          list(
            mean = function(x) round(x, digits = 3) |> as.character(),
            N = function(x) format(round(x, digits = 2), nsmall = 2),
            N_obs = function(x) format(round(x, digits = 2), nsmall = 2)
          )
      )
  ) |>
    apply_statistic_fmt_fn() |>
    dplyr::select(variable, stat_name, statistic, statistic_fmt) |>
    as.data.frame() |>
    expect_snapshot()
})

test_that("ard_continuous() messaging", {
  # proper error message when statistics argument mis-specified
  expect_snapshot(
    ard_continuous(mtcars, variables = "mpg", statistics = ~list(mean = "this is a string")),
    error = TRUE
  )

})

