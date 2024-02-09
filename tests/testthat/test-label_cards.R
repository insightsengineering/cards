test_that("label_cards() works", {
  expect_equal(
    label_cards(scale = 100, digits = 2)(9:10),
    c("900.00", "1000.00")
  )

  expect_equal(
    label_cards(digits = 2, width = 5)(9:10),
    c(" 9.00", "10.00")
  )

  expect_equal(
    label_cards()(NA),
    NA_character_
  )

  expect_equal(
    label_cards(width = 5)(c(NA, 1)),
    c(NA_character_, "  1.0")
  )
})
