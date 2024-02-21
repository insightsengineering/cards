test_that("tidy_as_ard() works", {
  # function works with standard use
  expect_snapshot(
    tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          # this mimics a tidier
          stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]])[c("estimate", "p.value", "method")] |>
            dplyr::as_tibble()
        ),
      tidy_result_names =
        c("estimate", "p.value", "method"),
      fun_args_to_record =
        c(
          "workspace", "hybrid", "hybridPars", "control", "or",
          "conf.int", "conf.level", "simulate.p.value", "B"
        ),
      formals = formals(stats::fisher.test),
      passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs")
    ) |>
      as.data.frame()
  )

  # function works when primary stats function errors
  expect_snapshot(
    tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          stop("Planned unit testing error!")
        ),
      tidy_result_names =
        c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
      fun_args_to_record =
        c(
          "workspace", "hybrid", "hybridPars", "control", "or",
          "conf.int", "conf.level", "simulate.p.value", "B"
        ),
      formals = formals(stats::fisher.test),
      passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs")
    ) |>
      as.data.frame()
  )

  # function works when `fun_args_to_record` argument is not passed.
  expect_snapshot(
    tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]])[c("estimate", "p.value", "method")] |>
            dplyr::as_tibble()
        ),
      tidy_result_names =
        c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
      formals = formals(stats::fisher.test),
      passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs")
    ) |>
      as.data.frame() |>
      dplyr::select(c(group1, variable, stat))
  )

  # function works when `formals` argument is not passed.
  expect_snapshot(
    tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]])[c("estimate", "p.value", "method")] |>
            dplyr::as_tibble()
        ),
      tidy_result_names =
        c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
      passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs")
    ) |>
      as.data.frame() |>
      dplyr::select(c(group1, variable, stat))
  )
})
