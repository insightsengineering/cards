test_that("ard_continuous() works", {
  expect_error(
    ard_test <-
      ard_continuous(mtcars, variables = c(mpg, hp), by = c(am, vs)),
    NA
  )
  expect_snapshot(class(ard_test))

  expect_equal(
    get_ard_statistics(
      ard_test,
      group1_level %in% 0,
      group2_level %in% 0,
      variable %in% "mpg",
      stat_name %in% c("N", "mean")
    ),
    list(
      N = with(mtcars, length(mpg[am %in% 0 & vs %in% 0])),
      mean = with(mtcars, mean(mpg[am %in% 0 & vs %in% 0]))
    ),
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
    statistic = list(AGE = continuous_summary_fns(c("N", "mean", "median"))),
    fmt_fn =
      list(
        AGE =
          list(
            mean = function(x) round5(x, digits = 3) |> as.character(),
            N = function(x) format(round5(x, digits = 2), nsmall = 2),
            N_obs = function(x) format(round5(x, digits = 2), nsmall = 2)
          )
      )
  ) |>
    apply_fmt_fn() |>
    dplyr::select(variable, stat_name, stat, stat_fmt) |>
    as.data.frame() |>
    expect_snapshot()

  ard_continuous(
    ADSL,
    variables = c("AGE", "BMIBL"),
    statistic = ~ continuous_summary_fns("mean"),
    fmt_fn =
      list(
        AGE =
          list(
            mean = function(x) round5(x, digits = 3) |> as.character()
          )
      )
  ) |>
    apply_fmt_fn() |>
    dplyr::select(variable, stat_name, stat, stat_fmt) |>
    as.data.frame() |>
    expect_snapshot()

  # tidyselect works
  ard_continuous(
    ADSL,
    variables = c("AGE", "BMIBL"),
    statistic = ~ continuous_summary_fns(c("mean", "sd")),
    fmt_fn = ~ list(~ function(x) round(x, 4))
  ) |>
    apply_fmt_fn() |>
    dplyr::select(variable, stat_name, stat, stat_fmt) |>
    as.data.frame() |>
    expect_snapshot()
})

test_that("ard_continuous() messaging", {
  # proper error message when statistic argument mis-specified
  expect_snapshot(
    ard_continuous(mtcars, variables = "mpg", statistic = ~ list(mean = "this is a string")),
    error = TRUE
  )

  # proper error message when non-data frame passed
  expect_snapshot(
    ard_continuous(letters, variables = "mpg"),
    error = TRUE
  )

  # proper error message when variables not passed
  expect_snapshot(
    ard_continuous(mtcars),
    error = TRUE
  )
})

test_that("ard_continuous(stat_label) argument works", {
  # formula
  expect_snapshot(
    ard_continuous(
      data = ADSL,
      by = "ARM",
      variables = c("AGE", "BMIBL"),
      stat_label = everything() ~ list(c("min", "max") ~ "min - max")
    ) |>
      as.data.frame() |>
      dplyr::select(stat_name, stat_label) |>
      dplyr::filter(stat_name %in% c("min", "max")) |>
      unique()
  )

  # list
  expect_snapshot(
    ard_continuous(
      data = ADSL,
      by = "ARM",
      variables = c("AGE", "BMIBL"),
      stat_label = everything() ~ list(p25 = "25th %ile", p75 = "75th %ile")
    ) |>
      as.data.frame() |>
      dplyr::select(stat_name, stat_label) |>
      dplyr::filter(stat_name %in% c("p25", "p75")) |>
      unique()
  )

  # variable-specific
  expect_snapshot(
    ard_continuous(
      data = ADSL,
      by = "ARM",
      variables = c("AGE", "BMIBL"),
      stat_label = AGE ~ list(p25 = "25th %ile", p75 = "75th %ile")
    ) |>
      as.data.frame() |>
      dplyr::filter(stat_name %in% c("p25", "p75")) |>
      dplyr::select(variable, stat_name, stat_label) |>
      unique()
  )

  # statistics returns a named list of summaries
  conf_int <- function(x) {
    t.test(x)[["conf.int"]] |>
      as.list() |>
      rlang::set_names(c("conf.low", "conf.high"))
  }
  ard1 <-
    ard_continuous(
      ADSL,
      variables = "AGE",
      statistic = ~ list(conf.int = conf_int),
      stat_label = ~ list(conf.low = "LB", conf.high = "UB")
    ) |>
    dplyr::select(variable, stat_name, stat_label) |>
    as.data.frame()

  expect_snapshot(ard1)

  ard2 <- ard_continuous(
    ADSL,
    variables = "AGE",
    statistic = ~ list(conf.int = conf_int),
    stat_label = ~ list("conf.low" ~ "LB", "conf.high" ~ "UB")
  ) |>
    dplyr::select(variable, stat_name, stat_label) |>
    as.data.frame()

  expect_equal(ard1, ard2)
})

test_that("ard_continuous() and ARD column names", {
  ard_colnames <- c(
    "group1", "group1_level", "variable", "variable_level",
    "context", "stat_name", "stat_label", "statistic",
    "fmt_fn", "warning", "error"
  )

  # no errors when these variables are the summary vars
  expect_error(
    {
      df <- mtcars
      names(df) <- ard_colnames
      ard_continuous(
        data = suppressMessages(cbind(mtcars["am"], df)),
        variables = all_of(ard_colnames),
        by = "am"
      )
    },
    NA
  )

  # no errors when these vars are the by var
  expect_error(
    {
      lapply(
        ard_colnames,
        function(byvar) {
          df <- mtcars[c("am", "mpg")]
          names(df) <- c(byvar, "mpg")
          ard_continuous(
            data = df,
            by = all_of(byvar),
            variables = "mpg"
          )
        }
      )
    },
    NA
  )
})


test_that("ard_continuous() with grouped data works", {
  expect_equal(
    ADSL |>
      dplyr::group_by(ARM) |>
      ard_continuous(variables = AGE),
    ard_continuous(data = ADSL, by = ARM, variables = AGE)
  )
})

test_that("ard_continuous() with dates works and displays as expected", {
  ard_date <- ADSL |>
    ard_continuous(
      variables = DISONSDT,
      statistic = ~ continuous_summary_fns(c("min", "max", "sd"))
    )
  expect_snapshot(ard_date)

  expect_equal(ard_date$stat[[1]], as.Date("1998-06-13"))
})

test_that("ard_continuous() with empty/missing dates works, and preserves Date class", {
  empty_date <- data.frame(dt = as.Date(NA)) |>
    ard_continuous(
      variables = dt,
      statistic = ~ continuous_summary_fns(c("min"))
    )
  expect_equal(inherits(empty_date$stat[[1]], "Date"), TRUE)
})
