test_that("apply_statistic_fmt_fn() works", {
  ard_fmt_checks <-
    ard_continuous(
      data = mtcars,
      variables = mpg,
      statistics = ~continuous_variable_summary_fns(c("mean", "sd"))
    ) |>
    dplyr::filter(stat_name %in% c("mean", "sd"))

  expect_equal(
    ard_fmt_checks |>
      dplyr::mutate(
        statistic_fmt_fn = list(2, 2)
      ) |>
      apply_statistic_fmt_fn() |>
      dplyr::pull(statistic_fmt) |>
      unlist(),
    c("20.09", "6.03")
  )

  expect_equal(
    ard_fmt_checks |>
      dplyr::mutate(
        statistic_fmt_fn = list("xx.xx", "xx.xx")
      ) |>
      apply_statistic_fmt_fn() |>
      dplyr::pull(statistic_fmt) |>
      unlist(),
    c("20.09", " 6.03")
  )
})

