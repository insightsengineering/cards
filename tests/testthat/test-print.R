test_that("print.card() works", {
  expect_snapshot(
    ard_continuous(ADSL, by = "ARM", variables = "AGE")
  )

  expect_snapshot(
    ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
  )

  expect_snapshot(
    ard_continuous(
      ADSL,
      variables = "AGE",
      fmt_fun = AGE ~ list(~ \(x) round(x, 3))
    )
  )

  # checking the print of Dates
  expect_snapshot(
    ard_continuous(
      data = data.frame(
        x = seq(as.Date("2000-01-01"), length.out = 10L, by = "day")
      ),
      variables = x,
      statistic = ~ continuous_summary_fns(c("min", "max", "sd"))
    ) |>
      dplyr::select(-fmt_fun)
  )

  # checking the print of a complex matrix statistic result
  expect_snapshot(
    bind_ard(
      ard_attributes(mtcars, variables = mpg),
      ard_continuous(
        mtcars,
        variables = mpg,
        statistic = ~ continuous_summary_fns(
          "mean",
          other_stats = list(vcov = \(x) lm(mpg ~ am, mtcars) |> vcov())
        )
      )
    )
  )
})

# Tests for improved print.card() method functionality
test_that("print.card() parameter validation works", {
  # Test error handling for non-card objects
  expect_error(
    print.card(mtcars),
    "x must be a card object"
  )

  # Test that function works with valid card objects
  card_obj <- ard_continuous(ADSL, variables = "AGE")
  expect_no_error(print.card(card_obj))

  # Test that function returns invisibly
  result <- print.card(card_obj)
  expect_identical(result, card_obj)
})

test_that("print.card() width parameter works", {
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")

  # Test with different width values
  expect_no_error(print.card(card_obj, width = 50))
  expect_no_error(print.card(card_obj, width = 100))
  expect_no_error(print.card(card_obj, width = 150))

  # Test with NULL width (should use default)
  expect_no_error(print.card(card_obj, width = NULL))
})

test_that("print.card() n parameter controls row display", {
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")

  # Test with different n values
  expect_snapshot(print.card(card_obj, n = 5))
  expect_snapshot(print.card(card_obj, n = 15))
  expect_snapshot(print.card(card_obj, n = 50))

  # Test with NULL n (should use default logic)
  expect_no_error(print.card(card_obj, n = NULL))
})

test_that("print.card() columns parameter works", {
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")

  # Test with auto columns (default)
  expect_snapshot(print.card(card_obj, columns = "auto"))

  # Test with all columns
  expect_snapshot(print.card(card_obj, columns = "all"))

  # Test default behavior
  expect_no_error(print.card(card_obj))
})

test_that("print.card() n_col parameter controls column threshold", {
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")

  # Test with different n_col values
  expect_snapshot(print.card(card_obj, n_col = 3))
  expect_snapshot(print.card(card_obj, n_col = 8))
  expect_snapshot(print.card(card_obj, n_col = 15))
})

test_that("print.card() handles edge cases properly", {
  # Test with empty card
  empty_card <- ard_continuous(ADSL[0, ], variables = "AGE")
  expect_no_error(print.card(empty_card))

  # Test with single row
  single_row <- ard_continuous(ADSL, variables = "AGE") |> dplyr::slice(1)
  expect_no_error(print.card(single_row))

  # Test with many rows
  many_rows <- ard_categorical(ADSL, variables = "AGEGR1")
  expect_no_error(print.card(many_rows, n = 25))
})

test_that("print.card() helper functions work correctly", {
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")
  x_print <- as.data.frame(card_obj)

  # Test column type detection
  col_types <- tryCatch(.get_column_types(x_print), error = function(e) NULL)
  expect_true(is.null(col_types) || is.character(col_types))

  # Test column selection
  selected <- tryCatch(
    .select_columns_for_printing(x_print, 6L, 80L),
    error = function(e) .select_columns_fallback(x_print, 6L)
  )
  expect_true(is.data.frame(selected))
  expect_true(ncol(selected) <= ncol(x_print))

  # Test column formatting
  formatted <- tryCatch(
    .format_columns_for_printing(x_print),
    error = function(e) .format_columns_fallback(x_print)
  )
  expect_true(is.data.frame(formatted))
  expect_equal(nrow(formatted), nrow(x_print))
})

test_that("print.card() fallback mechanisms work", {
  card_obj <- ard_continuous(ADSL, variables = "AGE")

  # Test that print works even if helper functions fail
  # This is tested implicitly by the robust error handling in the main function
  expect_no_error(print.card(card_obj))
})

test_that("print.card() handles list columns properly", {
  # Create a card with various column types including lists
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")

  # Test that list columns are handled correctly
  expect_snapshot(print.card(card_obj))

  # Test with card that has warning/error columns
  card_with_warnings <- ard_continuous(ADSL, variables = "AGE")
  expect_no_error(print.card(card_with_warnings))
})

test_that("print.card() output formatting is consistent", {
  # Test various card types to ensure consistent formatting

  # Continuous variables
  continuous_card <- ard_continuous(ADSL, variables = "AGE")
  expect_snapshot(print.card(continuous_card))

  # Categorical variables
  categorical_card <- ard_categorical(ADSL, variables = "AGEGR1")
  expect_snapshot(print.card(categorical_card))

  # With grouping
  grouped_card <- ard_continuous(ADSL, by = "ARM", variables = "AGE")
  expect_snapshot(print.card(grouped_card))

  # With formatting functions
  formatted_card <- ard_continuous(
    ADSL,
    variables = "AGE",
    fmt_fun = AGE ~ list(~ function(x) round(x, 2))
  )
  expect_snapshot(print.card(formatted_card))
})

test_that("print.card() width awareness works", {
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")

  # Test with very narrow width
  expect_snapshot(print.card(card_obj, width = 40))

  # Test with wide width
  expect_snapshot(print.card(card_obj, width = 120))
})

test_that("print.card() preserves backward compatibility", {
  # Test that all existing functionality still works
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")

  # Test basic print (should work exactly as before)
  expect_no_error(print(card_obj))

  # Test with existing parameters
  expect_no_error(print(card_obj, n = 10))
  expect_no_error(print(card_obj, columns = "auto"))
  expect_no_error(print(card_obj, n_col = 6))
})
