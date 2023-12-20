test_that("ard_crosstab(percent='cell') works", {
  expect_error(
    ard_crosstab <- ard_crosstab(ADSL, variables = "AGEGR1", by = "ARM"),
    NA
  )

  mtrx_conts <- with(ADSL, table(AGEGR1, ARM)) |> unclass()
  mtrx_percs <- mtrx_conts / sum(mtrx_conts)

  expect_equal(
    ard_crosstab |>
      dplyr::filter(group1_level %in% "Placebo", variable_level %in% "<65", stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      getElement(1),
    mtrx_conts["<65", "Placebo"]
  )

  expect_equal(
    ard_crosstab |>
      dplyr::filter(group1_level %in% "Placebo", variable_level %in% "<65", stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      getElement(1),
    mtrx_percs["<65", "Placebo"]
  )


  expect_equal(
    ard_crosstab(
      mtcars,
      by = am,
      variables = starts_with("xxxxx")
    ),
    dplyr::tibble()
  )
})

test_that("ard_crosstab(percent='row') works", {
  expect_error(
    ard_crosstab_row <- ard_crosstab(ADSL, variables = "AGEGR1", by = "ARM", percent = "row"),
    NA
  )

  xtab_count <- with(ADSL, table(AGEGR1, ARM))
  xtab_percent <- proportions(xtab_count, margin = 1)

  expect_equal(
    xtab_count[rownames(xtab_count) %in% "<65", colnames(xtab_count) %in% "Placebo"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% "<65", group1_level %in% "Placebo", stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    xtab_percent[rownames(xtab_percent) %in% "<65", colnames(xtab_percent) %in% "Placebo"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% "<65", group1_level %in% "Placebo", stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    xtab_count[rownames(xtab_count) %in% ">80", colnames(xtab_count) %in% "Xanomeline Low Dose"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% ">80", group1_level %in% "Xanomeline Low Dose", stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    xtab_percent[rownames(xtab_percent) %in% ">80", colnames(xtab_percent) %in% "Xanomeline Low Dose"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% ">80", group1_level %in% "Xanomeline Low Dose", stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      unlist(),
    ignore_attr = TRUE
  )

  # testing the arguments work properly
  expect_error(
    ard_with_args <-
      ard_crosstab(
        ADSL, variables = "AGEGR1", by = "ARM",
        percent = "row",
        statistics = list(AGEGR1 = categorical_variable_summary_fns(c("n", "N"))),
        fmt_fn = list(AGEGR1 = list("n" = 2))
      ),
    NA
  )

  expect_snapshot(
    ard_with_args |>
      apply_statistic_fmt_fn() |>
      flatten_ard() |>
      dplyr::select(-statistic_fmt_fn, -warning, -error) |>
      as.data.frame()
  )
})

test_that("ard_crosstab(percent='column') works", {
  expect_equal(
    ard_crosstab(ADSL, variables = "AGEGR1", by = "ARM", percent = "column") |>
      dplyr::select(all_ard_groups(), all_ard_variables(), stat_name, statistic),
    ard_categorical(ADSL, variables = "AGEGR1", by = "ARM") |>
      dplyr::select(all_ard_groups(), all_ard_variables(), stat_name, statistic)
  )
})
