test_that("check the proportion_ci_*() functions work", {
  # setting vectors to test
  x_dbl <- c(NA, mtcars$vs)
  x_lgl <- as.numeric(x_dbl)
  x_rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  x_true <- rep_len(TRUE, 32)
  x_false <- rep_len(FALSE, 32)

  # check Wilson CIs ----------------------------------------------------------
  expect_snapshot(
    wilson_dbl <- proportion_ci_wilson(x_dbl, conf.level = 0.9, correct = FALSE)
  )
  expect_equal(
    wilson_dbl[c("conf.low", "conf.high")],
    prop.test(x = sum(x_dbl, na.rm = TRUE), n = 32, conf.level = 0.9, correct = FALSE)$conf.int |>
      as.list() |>
      setNames(c("conf.low", "conf.high"))
  )

  expect_snapshot(
    wilsoncc_dbl <- proportion_ci_wilson(x_dbl, conf.level = 0.9, correct = TRUE)
  )
  expect_equal(
    wilsoncc_dbl[c("conf.low", "conf.high")],
    prop.test(x = sum(x_dbl, na.rm = TRUE), n = 32, conf.level = 0.9, correct = TRUE)$conf.int |>
      as.list() |>
      setNames(c("conf.low", "conf.high"))
  )

  expect_snapshot(
    wilson_lgl <- proportion_ci_wilson(x_lgl, conf.level = 0.9, correct = FALSE)
  )
  expect_equal(
    wilson_lgl[c("conf.low", "conf.high")],
    prop.test(x = sum(x_lgl, na.rm = TRUE), n = 32, conf.level = 0.9, correct = FALSE)$conf.int |>
      as.list() |>
      setNames(c("conf.low", "conf.high"))
  )

  expect_snapshot(proportion_ci_wilson(x_rsp))
  expect_snapshot(proportion_ci_wilson(x_true))
  expect_snapshot(proportion_ci_wilson(x_false))

})
