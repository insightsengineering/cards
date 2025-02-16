test_that("tidy_ard_column_order() works", {
  skip_if_not(is_pkg_installed("withr"))
  withr::local_seed(1)

  # ensure 10+ groups are ordered correctly
  expect_equal(
    data.frame(
      x1 = sample(LETTERS[1:2], 30, replace = TRUE),
      x2 = sample(LETTERS[3:4], 30, replace = TRUE),
      x3 = sample(LETTERS[5:6], 30, replace = TRUE),
      x4 = sample(LETTERS[7:8], 30, replace = TRUE),
      x5 = sample(LETTERS[9:10], 30, replace = TRUE),
      x6 = sample(LETTERS[11:12], 30, replace = TRUE),
      x7 = sample(LETTERS[13:14], 30, replace = TRUE),
      x8 = sample(LETTERS[15:16], 30, replace = TRUE),
      x9 = sample(LETTERS[17:18], 30, replace = TRUE),
      x10 = sample(LETTERS[19:20], 30, replace = TRUE),
      dummy = 1L
    ) |>
      ard_categorical(
        variables = "dummy",
        strata = x1:x10,
        statistic = everything() ~ "n"
      ) |>
      dplyr::select(all_ard_groups(), all_ard_variables()) |>
      names(),
    c(
      "group1", "group1_level",
      "group2", "group2_level",
      "group3", "group3_level",
      "group4", "group4_level",
      "group5", "group5_level",
      "group6", "group6_level",
      "group7", "group7_level",
      "group8", "group8_level",
      "group9", "group9_level",
      "group10", "group10_level",
      "variable", "variable_level"
    )
  )
})
