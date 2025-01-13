test_that("tidy_ard_row_order() works", {
  skip_if_not(is_pkg_installed("withr"))
  withr::local_options(list(width = 120))
  withr::local_seed(1)

  # ensure rows are ordered within descending groups but not variables
  expect_snapshot(
    data.frame(
      x1 = sample(LETTERS[1:5], 30, replace = TRUE),
      x2 = sample(LETTERS[6:10], 30, replace = TRUE),
      x3 = sample(LETTERS[11:15], 30, replace = TRUE),
      zz = 1L,
      aa = 1L
    ) |>
      ard_categorical(
        by = x1:x3,
        variables = c(zz, aa),
        statistic = everything() ~ "n"
      ) |>
      dplyr::select(all_ard_groups(), all_ard_variables())
  )
})
