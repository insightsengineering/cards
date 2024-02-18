test_that("print_ard_conditions() works", {
  # nothing prints with no errors/warnings
  expect_snapshot(
    ard_continuous(ADSL, variables = AGE) |>
      print_ard_conditions()
  )

  # expected messaging without by variable
  expect_snapshot(
    ard_continuous(
      ADSL,
      variables = AGE,
      statistic = ~ list(
        mean = \(x) mean(x),
        mean_warning = \(x) {
          warning("warn1")
          warning("warn2")
          mean(x)
        },
        err_fn = \(x) stop("'tis an error")
      )
    ) |>
      print_ard_conditions()
  )

  # expected messaging with by variable
  expect_snapshot(
    ard_continuous(
      ADSL,
      variables = AGE,
      by = ARM,
      statistic = ~ list(
        mean = \(x) mean(x),
        mean_warning = \(x) {
          warning("warn1")
          warning("warn2")
          mean(x)
        },
        err_fn = \(x) stop("'tis an error")
      )
    ) |>
      print_ard_conditions()
  )

  # expected messaging when the same error appears for all stats (consolidated correctly)
  expect_snapshot(
    ard_continuous(ADSL, variables = AGE) |>
      dplyr::mutate(error = list("repeated error")) |>
      print_ard_conditions()
  )

  # calling function name prints correctly
  expect_snapshot({
    tbl_summary <- function() {
      ard <- ard_continuous(
        ADSL,
        variables = AGE,
        statistic = ~ list(err_fn = \(x) stop("'tis an error"))
      )

      print_ard_conditions(ard, call = current_env())
    }
    tbl_summary()
  })
})
