skip_if_not(is_pkg_installed("withr"))

test_that("options(cards.round_type)", {
  # test that the p is rounded to zero (ie rounded to even) for aliases called by `apply_fmt_fun()`
  withr::local_options(list(cards.round_type = "round-to-even"))
  expect_equal(
    data.frame(x = c(T, F)) |>
      ard_categorical(variables = everything(), statistic = ~"p") |>
      update_ard_fmt_fun(stat_names = "p", fmt_fun = 0) |>
      apply_fmt_fun() |>
      dplyr::pull("stat_fmt") |>
      unique() |>
      unlist(),
    "0"
  )

  # test that the p is rounded to zero (ie rounded to even) for default fmt functions
  expect_equal(
    data.frame(x = rep_len(TRUE, 1999) |> c(FALSE)) |>
      ard_categorical(variables = everything(), statistic = ~"p") |>
      apply_fmt_fun() |>
      dplyr::filter(variable_level %in% FALSE) |>
      dplyr::pull("stat_fmt") |>
      unlist(),
    "0.0"
  )
})

test_that("options(cards.round_type) messaging", {
  # test message when the option is the wrong value
  expect_snapshot(
    error = TRUE,
    withr::with_options(
      list(cards.round_type = "NOT-CORRECT"),
      data.frame(x = c(T, F)) |>
        ard_categorical(variables = everything(), statistic = ~"p")
    )
  )
})
