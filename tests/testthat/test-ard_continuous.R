test_that("multiplication works", {
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
})
