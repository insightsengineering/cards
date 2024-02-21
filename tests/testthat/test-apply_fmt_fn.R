ard_fmt_checks <-
  ard_continuous(
    data = mtcars,
    variables = mpg,
    statistic = ~ continuous_summary_fns(c("mean", "sd"))
  )


test_that("apply_fmt_fn() works", {
  expect_equal(
    ard_fmt_checks |>
      apply_fmt_fn() |>
      dplyr::pull(stat_fmt) |>
      unlist(),
    c("20.1", "6.0")
  )

  # no errors when there is no formatting function
  expect_equal(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fn = list(NULL, 2)
      ) |>
      apply_fmt_fn() |>
      dplyr::pull(stat_fmt),
    list(NULL, "6.03")
  )
})

test_that("apply_fmt_fn() works with integer specification", {
  expect_equal(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fn = list(2, 2)
      ) |>
      apply_fmt_fn() |>
      dplyr::pull(stat_fmt) |>
      unlist(),
    c("20.09", "6.03")
  )
})

test_that("apply_fmt_fn() works with xx specification", {
  expect_equal(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fn = list("xx.xx", "xx.xx")
      ) |>
      apply_fmt_fn() |>
      dplyr::pull(stat_fmt) |>
      unlist(),
    c("20.09", " 6.03")
  )

  expect_equal(
    ard_categorical(
      data = mtcars,
      variables = am,
      fmt_fn = list(
        am = list(
          n = "xx",
          N = "xx",
          p = "xx.xx%"
        )
      )
    ) |>
      apply_fmt_fn() |>
      dplyr::pull(stat_fmt) |>
      unlist() |>
      unname(),
    c("19", "32", "59.38", "13", "32", "40.63")
  )
})

test_that("apply_fmt_fn() error messaging", {
  expect_snapshot(
    apply_fmt_fn(letters),
    error = TRUE
  )

  expect_snapshot(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fn = list("xoxo", "xoxo")
      ) |>
      apply_fmt_fn(),
    error = TRUE
  )

  expect_snapshot(
    ard_fmt_checks |>
      dplyr::mutate(
        fmt_fn = list(-1L, -1L)
      ) |>
      apply_fmt_fn(),
    error = TRUE
  )

  # everything still works when the formatted value is longer than the xxx string
  expect_snapshot(
    ard_fmt_checks |>
      dplyr::mutate(
        stat = lapply(stat, function(x) x * 1000),
        fmt_fn = list("xx", "xx")
      ) |>
      apply_fmt_fn() |>
      as.data.frame()
  )
})
