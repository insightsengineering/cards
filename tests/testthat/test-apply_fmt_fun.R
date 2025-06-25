ard_fmt_checks <-
  ard_continuous(
    data = mtcars,
    variables = mpg,
    statistic = ~ continuous_summary_fns(c("mean", "sd"))
  )


test_that("apply_fmt_fun() works", {
  expect_equal(
    ard_fmt_checks |>
      apply_fmt_fun() |>
      dplyr::pull(stat_fmt) |>
      unlist(),
    c("20.1", "6.0")
  )

  # no errors when there is no formatting function
  expect_equal(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fun = list(NULL, 2)
      ) |>
      apply_fmt_fun() |>
      dplyr::pull(stat_fmt),
    list(NULL, "6.03")
  )
})

test_that("apply_fmt_fun() works with integer specification", {
  expect_equal(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fun = list(2, 2)
      ) |>
      apply_fmt_fun() |>
      dplyr::pull(stat_fmt) |>
      unlist(),
    c("20.09", "6.03")
  )
})

test_that("apply_fmt_fun() works with xx specification", {
  expect_equal(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fun = list("xx.xx", "xx.xx")
      ) |>
      apply_fmt_fun() |>
      dplyr::pull(stat_fmt) |>
      unlist(),
    c("20.09", " 6.03")
  )

  expect_equal(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fun = list("xx.xxx", "xx.xxx")
      ) |>
      apply_fmt_fun() |>
      dplyr::pull(stat_fmt) |>
      unlist(),
    c("20.091", " 6.027")
  )

  expect_equal(
    ard_categorical(
      data = mtcars,
      variables = am,
      fmt_fun = list(
        am = list(
          n = "xx",
          N = "xx",
          p = "xx.xx%"
        )
      )
    ) |>
      apply_fmt_fun() |>
      dplyr::pull(stat_fmt) |>
      unlist() |>
      unname(),
    c("19", "32", "59.38", "13", "32", "40.63")
  )
})

test_that("apply_fmt_fun() error messaging", {
  expect_snapshot(
    apply_fmt_fun(letters),
    error = TRUE
  )

  expect_snapshot(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fun = list("xoxo", "xoxo")
      ) |>
      apply_fmt_fun(),
    error = TRUE
  )

  expect_snapshot(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fun = list(1L, -1L)
      ) |>
      apply_fmt_fun(),
    error = TRUE
  )

  # everything still works when the formatted value is longer than the xxx string
  expect_snapshot(
    ard_fmt_checks |>
      dplyr::mutate(
        stat = lapply(stat, function(x) x * 1000),
        fmt_fun = list("xx", "xx")
      ) |>
      apply_fmt_fun() |>
      as.data.frame()
  )
})

test_that("apply_fmt_fun(replace)", {
  ard <-
    ADSL |>
    ard_categorical(variables = AGEGR1, statistic = ~"n") |>
    dplyr::mutate(
      stat_fmt = ifelse(dplyr::row_number() == 1, list("144.000000"), list(NULL))
    )

  expect_snapshot(
    apply_fmt_fun(ard, replace = FALSE)
  )

  expect_snapshot(
    apply_fmt_fun(ard, replace = TRUE)
  )
})
