skip_if_not(is_pkg_installed("broom"))

test_that("ard_categorical_ci.data.frame(method = 'wald')", {
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "wald"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_wald(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'waldcc')", {
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "waldcc"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_wald(mtcars[["vs"]], correct = TRUE) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'clopper-pearson')", {
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "clopper-pearson"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_clopper_pearson(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'wilson')", {
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "wilson"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_wilson(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'wilsoncc')", {
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "wilsoncc"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_wilson(mtcars[["vs"]], correct = TRUE) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'strat_wilson')", {
  mtcars$gear <- as.factor(mtcars$gear)
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      strata = "gear",
      method = "strat_wilson"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_strat_wilson(mtcars[["vs"]], strata = as.factor(mtcars[["gear"]])) |>
      unlist() |> unname()
  )

  # check `denominator = "row"` result
  expect_equal(
    ard_categorical_ci(
      mtcars |> dplyr::mutate(cyl = factor(cyl)),
      variables = vs,
      by = am,
      strata = "cyl",
      method = "strat_wilson",
      denominator = "row"
    ) |>
      dplyr::filter(group1_level %in% 0) |>
      cards::get_ard_statistics(),
    proportion_ci_strat_wilson(
      x = (mtcars$am == 0)[mtcars$vs == 1],
      strata = as.factor(mtcars[["cyl"]])[mtcars$vs == 1]
    )
  )

  # check `denominator = "cell"` result
  expect_equal(
    ard_categorical_ci(
      mtcars |> dplyr::mutate(cyl = factor(cyl)),
      variables = vs,
      by = am,
      strata = "cyl",
      method = "strat_wilson",
      denominator = "cell"
    ) |>
      dplyr::filter(group1_level %in% 0, variable_level %in% 1) |>
      cards::get_ard_statistics(),
    proportion_ci_strat_wilson(
      x = mtcars$am == 0 & mtcars$vs == 1,
      strata = as.factor(mtcars[["cyl"]])
    )
  )
})

test_that("ard_categorical_ci.data.frame(method = 'strat_wilsoncc')", {
  mtcars$gear <- as.factor(mtcars$gear)
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      strata = "gear",
      method = "strat_wilsoncc"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_strat_wilson(mtcars[["vs"]], strata = as.factor(mtcars[["gear"]]), correct = TRUE) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'jeffreys')", {
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "jeffreys"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_jeffreys(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_categorical_ci.data.frame(method = 'agresti-coull')", {
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "agresti-coull"
    ) |>
      dplyr::select(stat) |> unlist() |> unname(),
    proportion_ci_agresti_coull(mtcars[["vs"]]) |>
      unlist() |> unname()
  )
})

test_that("ard_continuous_ci.data.frame() follows ard structure", {
  expect_silent(
    ard_categorical_ci(
      mtcars,
      variables = vs,
      method = "wald"
    ) |>
      cards::check_ard_structure()
  )
})


test_that("ard_continuous_ci.data.frame(denominator='row')", {
  # check the structure of the output
  expect_silent(
    ard <- ard_categorical_ci(
      mtcars,
      by = am,
      variables = vs,
      value = NULL,
      denominator = "row"
    )
  )
  expect_silent(
    cards::check_ard_structure(ard)
  )

  # check the estimates align with `cards::ard_categorical(denominator='row)`
  expect_equal(
    ard |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level)),
    cards::ard_categorical(
      mtcars,
      by = am,
      variables = vs,
      denominator = "row",
      statistic = ~"p"
    ) |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level))
  )

  # check the estimates align with `cards::ard_categorical(denominator='row)` for dichotomous variables
  expect_equal(
    ard_categorical_ci(
      mtcars,
      by = am,
      variables = vs,
      denominator = "row"
    ) |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level)),
    cards::ard_dichotomous(
      mtcars,
      by = am,
      variables = vs,
      denominator = "row",
      statistic = ~"p"
    ) |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level))
  )

  # check the results work with multiple `by` variables
  expect_equal(
    ard_categorical_ci(
      mtcars,
      by = c(cyl, gear),
      variables = am,
      denominator = "row"
    ) |>
      dplyr::filter(group1_level %in% 4, group2_level %in% 3) |>
      cards::get_ard_statistics(),
    proportion_ci_wald(
      x = (mtcars$cyl == 4 & mtcars$gear == 3)[mtcars$am == 1],
      correct = TRUE
    )
  )

  # check the results work with no `by` variables
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = am,
      denominator = "row"
    ) |>
      cards::get_ard_statistics(),
    proportion_ci_wald(
      x = rep_len(TRUE, length.out = sum(mtcars$am == 1)),
      correct = TRUE
    )
  )
})

test_that("ard_continuous_ci.data.frame(denominator='cell')", {
  # check the structure of the output
  expect_silent(
    ard <- ard_categorical_ci(
      mtcars,
      by = am,
      variables = vs,
      value = NULL,
      denominator = "cell"
    )
  )
  expect_silent(
    cards::check_ard_structure(ard)
  )

  # check the estimates align with `cards::ard_categorical(denominator='row)`
  expect_equal(
    ard |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(group1, variable, unlist(group1_level), unlist(variable_level)),
    cards::ard_categorical(
      mtcars,
      by = am,
      variables = vs,
      denominator = "cell",
      statistic = ~"p"
    ) |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(group1, variable, unlist(group1_level), unlist(variable_level))
  )

  # check the estimates align with `cards::ard_categorical(denominator='row)` for dichotomous variables
  expect_equal(
    ard_categorical_ci(
      mtcars,
      by = am,
      variables = vs,
      denominator = "cell"
    ) |>
      dplyr::filter(stat_name == "estimate") |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level)),
    cards::ard_dichotomous(
      mtcars,
      by = am,
      variables = vs,
      denominator = "cell",
      statistic = ~"p"
    ) |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables(), "stat") |>
      dplyr::arrange(unlist(group1_level), unlist(variable_level))
  )

  # check the results work with multiple `by` variables
  expect_equal(
    ard_categorical_ci(
      mtcars,
      by = c(cyl, gear),
      variables = am,
      denominator = "cell"
    ) |>
      dplyr::filter(group1_level %in% 4, group2_level %in% 3) |>
      cards::get_ard_statistics(),
    proportion_ci_wald(
      x = (mtcars$cyl == 4 & mtcars$gear == 3 & mtcars$am == 1),
      correct = TRUE
    )
  )

  # check the results work with no `by` variables
  expect_equal(
    ard_categorical_ci(
      mtcars,
      variables = am,
      denominator = "cell"
    ) |>
      cards::get_ard_statistics(),
    proportion_ci_wald(
      x = mtcars$am == 1,
      correct = TRUE
    )
  )
})

test_that("ard_continuous_ci.data.frame(denominator='column') with all NA column", {
  expect_silent(
    ard <-
      mtcars |>
      dplyr::mutate(vs = ifelse(am == 0, NA, vs)) |>
      ard_categorical_ci(
        variables = vs,
        by = am,
        denominator = "column",
        method = "wilson"
      )
  )

  expect_equal(
    ard |>
      dplyr::filter(group1_level %in% 0) |>
      dplyr::pull(stat_name),
    c("N", "n", "estimate", "conf.low", "conf.high", "conf.level", "method")
  )
})

test_that("ard_continuous_ci.data.frame(denominator='row') with all NA column", {
  expect_silent(
    ard <-
      mtcars |>
      dplyr::mutate(vs = ifelse(am == 0, NA, vs)) |>
      ard_categorical_ci(
        variables = vs,
        by = am,
        denominator = "row",
        method = "wilson"
      )
  )

  expect_equal(
    ard |>
      dplyr::filter(group1_level %in% 0) |>
      dplyr::pull(stat_name),
    c("N", "n", "conf.level", "estimate", "statistic", "p.value", "parameter", "conf.low", "conf.high", "method", "alternative")
  )
})

test_that("ard_continuous_ci.data.frame(denominator='cell') with all NA column", {
  expect_silent(
    ard <-
      mtcars |>
      dplyr::mutate(vs = ifelse(am == 0, NA, vs)) |>
      ard_categorical_ci(
        variables = vs,
        by = am,
        denominator = "cell",
        method = "wilson"
      )
  )

  expect_equal(
    ard |>
      dplyr::filter(group1_level %in% 0) |>
      dplyr::pull(stat_name),
    c("N", "n", "conf.level", "estimate", "statistic", "p.value", "parameter", "conf.low", "conf.high", "method", "alternative")
  )
})

test_that("ard_continuous_ci.data.frame() NA handling", {
  df <- mtcars[c("am", "cyl", "gear")]
  df$am[1:5] <- NA
  df$cyl[6:10] <- NA

  expect_equal(
    ard_categorical_ci(df, by = am, variables = cyl, denominator = "column", method = "wald") |>
      dplyr::filter(group1_level %in% 0, variable_level %in% 4) |>
      cards::get_ard_statistics(),
    proportion_ci_wald((df$cyl == 4)[df$am == 0])
  )

  expect_equal(
    ard_categorical_ci(df, by = am, variables = cyl, denominator = "row", method = "wald") |>
      dplyr::filter(group1_level %in% 0, variable_level %in% 4) |>
      cards::get_ard_statistics(),
    proportion_ci_wald((df$am == 0)[df$cyl == 4])
  )

  expect_equal(
    ard_categorical_ci(df, by = am, variables = cyl, denominator = "cell", method = "wald") |>
      dplyr::filter(group1_level %in% 0, variable_level %in% 4) |>
      cards::get_ard_statistics(),
    proportion_ci_wald((df$am == 0) + (df$cyl == 4) > 1)
  )
})
