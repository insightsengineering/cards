test_that("replace_null_statistic() works", {
  expect_error(
    ard_with_missing_stats <-
      data.frame(x = rep_len(NA_character_, 10)) |>
      ard_continuous(
        variables = x,
        statistic = ~ continuous_summary_fns(c("median", "p25", "p75"))
      ) |>
      replace_null_statistic(rows = !is.null(error)),
    NA
  )

  # all results should now be NA_character
  expect_equal(
    ard_with_missing_stats$stat |> unlist() |> unique(),
    NA_character_
  )
})
