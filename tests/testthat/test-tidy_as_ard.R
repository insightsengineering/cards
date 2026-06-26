test_that("tidy_as_ard() works", {
  fun_args_to_record <-
    setdiff(names(formals(stats::fisher.test)), c("x", "y", "alternative"))

  # function works with standard use
  ard <-
    tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          # this mimics a tidier
          stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]])[c("estimate", "p.value", "method")] |>
            dplyr::as_tibble()
        ),
      tidy_result_names =
        c("estimate", "p.value", "method"),
      fun_args_to_record = fun_args_to_record,
      formals = formals(stats::fisher.test),
      passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs")
    )
  fisher_result <-
    stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]])[c("estimate", "p.value", "method")]
  expect_equal(
    get_ard_statistics(ard, .column = "stat"),
    c(
      list(
        estimate = fisher_result[["estimate"]],
        p.value = fisher_result[["p.value"]],
        method = fisher_result[["method"]]
      ),
      as.list(formals(stats::fisher.test)[fun_args_to_record])
    ),
    ignore_attr = TRUE
  )

  # function works when primary stats function errors
  ard <-
    tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          stop("Planned unit testing error!")
        ),
      tidy_result_names =
        c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
      fun_args_to_record = fun_args_to_record,
      formals = formals(stats::fisher.test),
      passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs")
    )
  expect_equal(
    get_ard_statistics(ard, .column = "stat"),
    c(
      stats::setNames(
        rep_len(list(NULL), 6L),
        c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative")
      ),
      as.list(formals(stats::fisher.test)[fun_args_to_record])
    ),
    ignore_attr = TRUE
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
