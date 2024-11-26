test_that("as_cards_fn() works", {
  expect_silent(
    fn <- as_cards_fn(\(x) list(one = 1, two = 2), stat_names = c("one", "two"))
  )
  expect_s3_class(fn, "cards_fn")
  expect_equal(get_cards_fn_stat_names(fn), c("one", "two"))
})
