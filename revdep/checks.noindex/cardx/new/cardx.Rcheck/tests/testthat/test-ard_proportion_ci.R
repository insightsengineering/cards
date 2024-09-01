skip_if_not(do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx")))

test_that("ard_categorical_ci() works", {
  # testing the easy methods together for binary variables
  expect_error(
    lst_ard_props <-
      c(
        "waldcc", "wald", "clopper-pearson",
        "wilson", "wilsoncc", "agresti-coull", "jeffreys"
      ) |>
      lapply(
        \(x) {
          ard_categorical_ci(
            data = mtcars,
            variables = c(am, vs),
            method = x
          )
        }
      ),
    NA
  )
  expect_equal(
    lst_ard_props[[1]] |>
      cards::get_ard_statistics(
        stat_name %in% c("estimate", "conf.low", "conf.high"),
        variable == "am"
      ),
    proportion_ci_wald(mtcars$am, correct = TRUE)[c("estimate", "conf.low", "conf.high")]
  )

  # testing a categorical variable
  expect_error(
    ard_factor <-
      ard_categorical_ci(
        mtcars |> dplyr::mutate(cyl = factor(cyl, levels = c(4, 6, 8, 10))),
        variables = cyl,
        by = am
      ),
    NA
  )
  expect_equal(
    cards::get_ard_statistics(
      ard_factor,
      group1_level %in% 0,
      map_lgl(variable_level, ~ .x == "4")
    )[c("estimate", "conf.low", "conf.high")],
    proportion_ci_wald(mtcars$cyl[mtcars$am == 0] == 4, correct = TRUE)[c("estimate", "conf.low", "conf.high")]
  )
  # now checking the unobserved level of cyl
  expect_equal(
    cards::get_ard_statistics(
      ard_factor,
      group1_level %in% 0,
      unlist(variable_level) == "10"
    )[c("estimate", "conf.low", "conf.high")],
    proportion_ci_wald(mtcars$cyl[mtcars$am == 0] == 10, correct = TRUE)[c("estimate", "conf.low", "conf.high")]
  )
  # checking structure
  expect_silent(cards::check_ard_structure(ard_factor))
})

test_that("ard_categorical_ci(method='strat_wilson') works", {
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

  expect_error(
    ard_categorical_ci_strat_wilson <-
      ard_categorical_ci(
        data = data.frame(
          rsp = rsp,
          strata = interaction(strata_data)
        ),
        variables = rsp,
        strata = strata,
        weights = weights,
        max.iterations = 10,
        method = "strat_wilson"
      ),
    NA
  )
  expect_snapshot(ard_categorical_ci_strat_wilson)

  expect_error(
    ard_categorical_ci_strat_wilsoncc <-
      ard_categorical_ci(
        data = data.frame(
          rsp = rsp,
          strata = interaction(strata_data)
        ),
        variables = rsp,
        strata = strata,
        weights = weights,
        max.iterations = 10,
        method = "strat_wilsoncc"
      ),
    NA
  )
  expect_snapshot(ard_categorical_ci_strat_wilsoncc)
})

test_that("ard_categorical_ci() messaging", {
  expect_snapshot(
    ard <- ard_categorical_ci(
      data = mtcars,
      variables = cyl,
      value = cyl ~ 10,
      method = "jeffreys"
    )
  )
})
