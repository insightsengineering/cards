test_that("as_card() works", {
  expect_snapshot(
    data.frame(
      stat_name = c("N", "mean"),
      stat_label = c("N", "Mean"),
      stat = c(10, 0.5)
    ) |>
      as_card()
  )
})

test_that("as_card() does not affect 'card' objects", {
  my_ard <- ard_continuous(ADSL, by = "ARM", variables = "AGE")

  expect_identical(
    my_ard |> as_card(),
    my_ard
  )
})

test_that("as_card() error catching works correctly", {
  expect_snapshot(
    "notadataframe" |>
      as_card(),
    error = TRUE
  )
})
