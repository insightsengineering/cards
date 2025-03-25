test_that("ard_formals() works", {
  expect_snapshot(
    ard_formals(fun = mcnemar.test, arg_names = "correct")
  )

  expect_snapshot(
    ard_formals(
      fun = asNamespace("stats")[["t.test.default"]],
      arg_names = c("mu", "paired", "var.equal", "conf.level"),
      passed_args = list(conf.level = 0.90)
    )
  )
})
