test_that("ard_complex() works", {
  # we can replicate `ard_continuous()` for univariate analysis
  # using the `x` arg in the mean function
  expect_equal(
    ard_complex(
      ADSL,
      by = "ARM",
      variables = "AGE",
      statistics = list(AGE = list(mean = \(x, ...) mean(x)))
    ) |>
      dplyr::select(all_ard_groups(), all_ard_variables(), statistic),
    ard_continuous(
      ADSL,
      by = "ARM",
      variables = "AGE",
      statistics = ~continuous_variable_summary_fns("mean")
    ) |>
      dplyr::select(all_ard_groups(), all_ard_variables(), statistic)
  )

  # using the `data` and `variable` args in the mean function
  expect_equal(
    ard_complex(
      ADSL,
      by = "ARM",
      variables = "AGE",
      statistics = list(AGE = list(mean = \(data, variable, ...) mean(data[[variable]])))
    ) |>
      dplyr::select(all_ard_groups(), all_ard_variables(), statistic),
    ard_continuous(
      ADSL,
      by = "ARM",
      variables = "AGE",
      statistics = ~continuous_variable_summary_fns("mean")
    ) |>
      dplyr::select(all_ard_groups(), all_ard_variables(), statistic)
  )

  # test a function using `data` and `data_full` arguments
  expect_error({
    grand_mean <- function(data, data_full, variable, ...) {
      list(mean = mean(data[[variable]], na.rm = TRUE),
           grand_mean = mean(data_full[[variable]], na.rm = TRUE))
    }
    ard_grand_mean <-
      ard_complex(
        ADSL,
        by = "ARM",
        variables = "AGE",
        statistics = list(AGE = list(means = grand_mean))
      ) |>
      as.data.frame() |>
      dplyr::select(all_ard_groups(), all_ard_variables(), stat_name, statistic)},
    NA
  )
  expect_equal(
    ard_grand_mean |>
      dplyr::filter(stat_name %in% "grand_mean") |>
      dplyr::pull(statistic) |>
      unique() |>
      getElement(1L),
    mean(ADSL$AGE)
  )
  expect_equal(
    ard_grand_mean |>
      as.data.frame() |>
      dplyr::filter(stat_name %in% "mean") |>
      dplyr::mutate(across(c(group1_level, statistic), unlist)) |>
      dplyr::select(group1_level, statistic),
    ADSL |>
      dplyr::summarise(
        .by = "ARM",
        statistic = mean(AGE)
      ) |>
      dplyr::rename(group1_level = ARM) |>
      as.data.frame(),
    ignore_attr = TRUE
  )
})

test_that("ard_complex() messaging", {
  # correct messaging when BMIBL doesn't have any summary fns
  expect_snapshot(
    error = TRUE,
    ard_complex(
      ADSL,
      by = "ARM",
      variables = c("AGE", "BMIBL"),
      statistics = list(AGE = list(mean = \(x, ...) mean(x)))
    )
  )
})
