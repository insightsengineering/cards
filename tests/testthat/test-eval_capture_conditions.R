test_that("eval_capture_conditions() works", {
  # no errors
  expect_snapshot(
    eval_capture_conditions(
      expr(TRUE)
    )
  )

  # capture the error
  expect_snapshot(
    eval_capture_conditions(
      expr(cli::cli_abort("BIG ERROR"))
    )
  )

  # capture warning
  expect_snapshot({
    one_warn_foo <- function() {
      cli::cli_warn("BIG WARNING")
      TRUE
    }
    eval_capture_conditions(expr(one_warn_foo()))
  })

  # capture multiple warning
  expect_snapshot({
    two_warn_foo <- function() {
      cli::cli_warn("{.emph BIG} WARNING1")
      cli::cli_warn("{.emph BIG} WARNING2")
      TRUE
    }
    eval_capture_conditions(expr(two_warn_foo()))
  })
})

# captured_condition_as_message() ----------------------------------------------
test_that("captured_condition_as_message() works", {
  # we get the result back when there is no error or warning
  expect_equal(
    eval_capture_conditions(letters) |>
      captured_condition_as_message(),
    letters
  )

  # print error as message with curly brackets in it
  expect_snapshot(
    eval_capture_conditions(stop("This is an {error}!")) |>
      captured_condition_as_message()
  )

  # print multiple warnings
  expect_snapshot(
    eval_capture_conditions({
      warning("This is a {warning} 1")
      warning("This is a {warning} 2")
      NULL
    }) |>
      captured_condition_as_message(type = "warning")
  )
})

# captured_condition_as_error() ----------------------------------------------
test_that("captured_condition_as_error() works", {
  # we get the result back when there is no error or warning
  expect_equal(
    eval_capture_conditions(letters) |>
      captured_condition_as_error(),
    letters
  )

  # print error as message with curly brackets in it
  expect_snapshot(
    error = TRUE,
    eval_capture_conditions(stop("This is an {error}!")) |>
      captured_condition_as_error()
  )

  # print multiple warnings
  expect_snapshot(
    error = TRUE,
    eval_capture_conditions({
      warning("This is a {warning} 1")
      warning("This is a {warning} 2")
      NULL
    }) |>
      captured_condition_as_error(type = "warning")
  )
})
