test_that("ard_proportion_ci() works", {
  # testing the easy methods together
  expect_error(
    c("waldcc", "wald", "clopper-pearson",
      "wilson", "wilsoncc", "agresti-coull", "jeffreys") |>
      map(
        ~ard_proportion_ci(
          data = mtcars,
          variables = c(am, vs),
          method = .x
        )
      ),
    NA
  )
})

test_that("ard_proportion_ci(method='strat_wilson') works", {
  withr::local_seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A"))
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 80, TRUE),
    "f2" = sample(c("x", "y", "z"), 80, TRUE),
    stringsAsFactors = TRUE
  )

  weights <- 1:6 / sum(1:6)

  expect_snapshot(
    ard_proportion_ci(
      data = data.frame(
        rsp = rsp,
        strata = interaction(strata_data)
      ),
      variables = rsp,
      strata = strata,
      weights = weights,
      method = 'strat_wilson'
    )
  )

  expect_snapshot(
    ard_proportion_ci(
      data = data.frame(
        rsp = rsp,
        strata = interaction(strata_data)
      ),
      variables = rsp,
      strata = strata,
      weights = weights,
      method = 'strat_wilsoncc'
    )
  )
})
