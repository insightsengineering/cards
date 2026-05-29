test_that("print_ard_conditions() works", {
  # nothing prints with no errors/warnings

  ard <- ard_summary(ADSL, variables = AGE)
  expect_null(print_ard_conditions(ard))
  expect_snapshot(print_ard_conditions(ard))

  # expected messaging without by variable
  ard <- ard_summary(
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
  )
  expect_no_error(ard)
  expect_message(
    expect_message(
      expect_message(
        print_ard_conditions(ard),
        "were returned during"
      ),
      "tis an error"
    ),
    "were returned during"
  )
  expect_snapshot(print_ard_conditions(ard))

  # expected messaging with by variable
  ard <- ard_summary(
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
  )
  expect_no_error(ard)
  expect_message(
    expect_message(
      print_ard_conditions(ard),
      "were returned during"
    ),
    "were returned during"
  )
  expect_snapshot(print_ard_conditions(ard))

  # expected messaging when the same error appears for all stats (consolidated correctly)
  expect_snapshot(
    ard_summary(ADSL, variables = AGE) |>
      dplyr::mutate(error = list("repeated error")) |>
      print_ard_conditions()
  )

  # calling function name prints correctly
  tbl_summary <- function() {
    set_cli_abort_call()

    ard <- ard_summary(
      ADSL,
      variables = AGE,
      statistic = ~ list(err_fn = \(x) stop("'tis an error"))
    )

    print_ard_conditions(ard)
  }
  expect_message(tbl_summary(), "tbl_summary")
  expect_snapshot(tbl_summary())
})

test_that("print_ard_conditions(condition_type)", {
  # expected warnings as warnings
  ard <- ard_summary(
    ADSL,
    variables = AGE,
    statistic = ~ list(mean_warning = \(x) {
      warning("warn1")
      warning("warn2")
      mean(x)
    })
  )
  expect_warning(
    expect_warning(
      expect_message(
        print_ard_conditions(ard, condition_type = "identity"),
        "print_ard_conditions()"
      ),
      "warn1"
    ),
    "warn2"
  )
  expect_snapshot(print_ard_conditions(ard, condition_type = "identity"))

  # expected warnings as warnings
  ard <- ard_summary(
    ADSL,
    variables = AGE,
    statistic = ~ list(
      mean = \(x) mean(x),
      err_fn = \(x) stop("'tis an error")
    )
  )
  expect_error(
    expect_message(
      print_ard_conditions(ard, condition_type = "identity"),
      "print_ard_conditions()"
    ),
    "tis an error"
  )
  expect_snapshot(
    error = TRUE,
    print_ard_conditions(ard, condition_type = "identity")
  )
})

test_that("print_ard_conditions() no error when 'error'/'warning' columns not present", {
  ard <- ard_summary(
    ADSL,
    variables = AGE,
    statistic = ~ list(
      mean = \(x) mean(x),
      err_fn = \(x) stop("'tis an error")
    )
  ) |>
    dplyr::select(-warning, -error)
  expect_no_error(print_ard_conditions(ard))
  expect_snapshot(print_ard_conditions(ard))
})

test_that("print_ard_conditions() no error when factors are present", {
  ard <-
    structure(list(
      group1 = c("by_var", "by_var"), group1_level = list(
        structure(1L, levels = c("cohort_1", "cohort_2"), class = "factor"),
        structure(1L, levels = c("cohort_1", "cohort_2"), class = "factor")
      ),
      variable = c("continuous_var", "continuous_var"), variable_level = list(
        NULL, NULL
      ), context = c("continuous", "continuous"),
      stat_name = c("min", "max"), stat_label = c("Min", "Max"),
      stat = list(Inf, -Inf), fmt_fun = list(1L, 1L), warning = list(
        "no non-missing arguments to min; returning Inf",
        "no non-missing arguments to max; returning -Inf"
      ), error = list(
        NULL, NULL
      )
    ), row.names = c(NA, -2L), class = c(
      "card",
      "tbl_df", "tbl", "data.frame"
    ))

  expect_snapshot(
    print_ard_conditions(ard)
  )
})

# See issue #309
test_that("print_ard_conditions() works when curly brackets appear in condition message", {
  # add a warning message that has curly brackets in it
  ard <- ard_summary(ADSL, variables = AGE, statistic = ~ continuous_summary_fns("mean")) |>
    dplyr::mutate(
      warning = list("warning with {curly} brackets"),
      error = list("error with {curly} brackets")
    )

  expect_snapshot(
    print_ard_conditions(ard)
  )
})
