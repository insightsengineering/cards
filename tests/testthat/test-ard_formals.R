test_that("ard_formals() works", {
  out <- ard_formals(fun = mcnemar.test, arg_names = "correct")
  expect_s3_class(out, "card")
  expect_named(out, c("stat_name", "stat_label", "stat"))
  expect_true(nrow(out) == 1L)
  expect_snapshot(out)


  out <- ard_formals(
    fun = asNamespace("stats")[["t.test.default"]],
    arg_names = c("mu", "paired", "var.equal", "conf.level"),
    passed_args = list(conf.level = 0.90)
  )
  expect_s3_class(out, "card")
  expect_named(out, c("stat_name", "stat_label", "stat"))
  expect_true(nrow(out) == 4L)
  expect_snapshot(out)
})
