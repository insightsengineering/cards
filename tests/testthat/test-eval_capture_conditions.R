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
