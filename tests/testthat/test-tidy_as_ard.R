describe("tidy_as_ard() works", {
  it("with standard use", {
    # function works

    ard <- tidy_as_ard(
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
    )
    expect_s3_class(ard, "card")
    expect_named(ard, c(
      "group1", "variable", "context", "stat_name", "stat", "fmt_fun",
      "warning", "error"
    ))
    expect_snapshot(ard)
  })

  it("when primary stats function errors", {
    ard <- tidy_as_ard(
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
    )
    expect_s3_class(ard, "card")
    expect_named(ard, c(
      "group1", "variable", "context", "stat_name", "stat", "fmt_fun",
      "warning", "error"
    ))
    expect_snapshot(ard)
  })
  it("when `fun_args_to_record` argument is not passed", {
    ard <- tidy_as_ard(
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
    )
    expect_s3_class(ard, "card")
    expect_named(ard, c(
      "group1", "variable", "context", "stat_name", "stat", "fmt_fun",
      "warning", "error"
    ))
    expect_snapshot(ard)
  })

  it("when `formals` argument is not passed.", {
    ard <- tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]])[c("estimate", "p.value", "method")] |>
            dplyr::as_tibble()
        ),
      tidy_result_names =
        c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
      passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs")
    )
    expect_s3_class(ard, "card")
    expect_named(ard, c(
      "group1", "variable", "context", "stat_name", "stat", "fmt_fun",
      "warning", "error"
    ))
    expect_snapshot(ard)
  })
})
