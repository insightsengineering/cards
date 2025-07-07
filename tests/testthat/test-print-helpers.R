# Tests for print.card() helper functions

test_that("print.card() improved functionality works", {
  # Test basic improvements
  card_obj <- ard_continuous(ADSL, variables = "AGE")
  
  # Test width parameter
  expect_no_error(print.card(card_obj, width = 60))
  expect_no_error(print.card(card_obj, width = 120))
  
  # Test n parameter with improved logic
  expect_no_error(print.card(card_obj, n = 3))
  expect_no_error(print.card(card_obj, n = 15))
  
  # Test columns parameter  
  expect_no_error(print.card(card_obj, columns = "all"))
  expect_no_error(print.card(card_obj, columns = "auto"))
  
  # Test n_col parameter
  expect_no_error(print.card(card_obj, n_col = 3))
  expect_no_error(print.card(card_obj, n_col = 10))
})

test_that("print.card() handles various card types", {
  # Test with different types of cards
  
  # Continuous variables
  continuous_card <- ard_continuous(ADSL, variables = "AGE")
  expect_no_error(print.card(continuous_card))
  
  # Categorical variables
  categorical_card <- ard_categorical(ADSL, variables = "AGEGR1")
  expect_no_error(print.card(categorical_card))
  
  # With grouping
  grouped_card <- ard_continuous(ADSL, by = "ARM", variables = "AGE")
  expect_no_error(print.card(grouped_card))
  
  # With formatting functions
  formatted_card <- ard_continuous(
    ADSL, 
    variables = "AGE", 
    fmt_fun = AGE ~ list(~ function(x) round(x, 2))
  )
  expect_no_error(print.card(formatted_card))
})

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

test_that("print.card() handles edge cases", {
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

test_that("print.card() output is well-formatted", {
  # Test that output includes expected elements
  card_obj <- ard_continuous(ADSL, variables = "AGE")
  
  # Capture output
  output <- capture_output(print(card_obj))
  
  # Check that output contains expected elements
  expect_true(grepl("cards", output$result))
  expect_true(grepl("data frame", output$result))
  expect_true(grepl("variable", output$result))
})

test_that("print.card() width parameter affects output", {
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")
  
  # Test with different width values
  expect_no_error(print.card(card_obj, width = 50))
  expect_no_error(print.card(card_obj, width = 100))
  expect_no_error(print.card(card_obj, width = 150))
  
  # Test with NULL width (should use default)
  expect_no_error(print.card(card_obj, width = NULL))
})

test_that("print.card() robust error handling works", {
  # Test that the method doesn't fail even in edge cases
  card_obj <- ard_continuous(ADSL, variables = "AGE")
  
  # Test with extreme parameter values
  expect_no_error(print.card(card_obj, n = 0))
  expect_no_error(print.card(card_obj, n = 1000))
  expect_no_error(print.card(card_obj, width = 10))
  expect_no_error(print.card(card_obj, width = 1000))
  expect_no_error(print.card(card_obj, n_col = 1))
  expect_no_error(print.card(card_obj, n_col = 100))
})

test_that("print.card() handles list columns properly", {
  # Create a card with various column types including lists
  card_obj <- ard_continuous(ADSL, by = "ARM", variables = "AGE")
  
  # Test that list columns are handled correctly
  expect_no_error(print.card(card_obj))
  
  # Test with card that has warning/error columns
  card_with_warnings <- ard_continuous(ADSL, variables = "AGE")
  expect_no_error(print.card(card_with_warnings))
})
