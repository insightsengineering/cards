test_that("as_card() allows non card objects with check = FALSE", {
  expect_snapshot(
    data.frame(
      stat_name = c("N", "mean"),
      stat_label = c("N", "Mean"),
      stat = c(10, 0.5)
    ) |>
      as_card(check = FALSE)
  )
})

test_that("as_card() does not affect 'card' objects", {
  my_ard <- ard_summary(ADSL, by = "ARM", variables = "AGE")

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
  nonliststat <- dplyr::tibble(
    variable = "AGE",
    stat_name = c("N", "mean"),
    stat_label = c("N", "Mean"),
    stat = c(10, 0.5),
    fmt_fun = replicate(2, list()),
    warning = replicate(2, list()),
    error = replicate(2, list())
  )

  expect_snapshot(
    nonliststat |>
      as_card(),
    error = TRUE
  )
})

test_that("as_card() does not care about column order", {

  badlyordered <- dplyr::tibble(
    stat_label = c("N", "Mean"),
    variable = "AGE",
    stat_name = c("N", "mean"),
    stat = list(10, 0.5),
    fmt_fun = replicate(2, list()),
    warning = replicate(2, list()),
    error = replicate(2, list())
  )

  expect_no_error(
    badlyordered |>
      as_card()
  )
})
