test_that("tidy_as_ard() works", {
  # function works with standard use
  expect_snapshot(
    tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]]) |>
            broom::tidy()
        ),
      tidy_result_names =
        c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
      fun_args_to_record =
        c("workspace", "hybrid", "hybridPars", "control", "or",
          "conf.int", "conf.level", "simulate.p.value", "B"),
      formals = formals(stats::fisher.test),
      passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs")
    ) |>
      as.data.frame()
  )

  # function works when proimary stats function errors
  expect_snapshot(
    tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          stop("Planned unit testing error!")
        ),
      tidy_result_names =
        c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
      fun_args_to_record =
        c("workspace", "hybrid", "hybridPars", "control", "or",
          "conf.int", "conf.level", "simulate.p.value", "B"),
      formals = formals(stats::fisher.test),
      passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs")
    ) |>
      as.data.frame()
  )
})
