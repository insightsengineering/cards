skip_if_not(is_pkg_installed("broom", reference_pkg = "cardx"))

test_that("ard_stats_wilcox_test() works", {
  # One Sample Wilcox works
  expect_error(
    ard_single <- ard_stats_wilcox_test(cards::ADSL, variable = AGE),
    NA
  )

  expect_equal(
    ard_single |>
      cards::get_ard_statistics(stat_name %in% c("statistic", "p.value")),
    wilcox.test(
      cards::ADSL$AGE
    ) |>
      broom::tidy() |>
      dplyr::select(statistic, p.value) |>
      unclass(),
    ignore_attr = TRUE
  )

  # Two Sample Wilcox works
  expect_error(
    ard_wilcoxtest <-
      cards::ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_stats_wilcox_test(by = ARM, variable = AGE, correct = FALSE, conf.int = TRUE),
    NA
  )

  expect_equal(
    ard_wilcoxtest |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    wilcox.test(
      AGE ~ ARM,
      data = cards::ADSL |> dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")),
      conf.int = TRUE,
      correct = FALSE
    ) |>
      broom::tidy() |>
      dplyr::select(estimate, conf.low, conf.high) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_equal(
    cards::ADSL |>
      ard_stats_wilcox_test(by = ARM, variable = AGE, correct = FALSE) |>
      dplyr::pull(error) |>
      getElement(1L),
    "grouping factor must have exactly 2 levels"
  )

  # test that the function works with multiple variables as once
  expect_equal(
    dplyr::bind_rows(
      ard_wilcoxtest,
      cards::ADSL |>
        dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
        ard_stats_wilcox_test(by = ARM, variable = BMIBL, correct = FALSE, conf.int = TRUE)
    ),
    cards::ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_stats_wilcox_test(by = ARM, variable = c(AGE, BMIBL), correct = FALSE, conf.int = TRUE)
  )
})

test_that("ard_stats_paired_wilcox_test() works", {
  ADSL_paired <-
    cards::ADSL[c("ARM", "AGE")] |>
    dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
    dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number())

  expect_error(
    ard_paired_wilcoxtest <-
      ADSL_paired |>
      ard_stats_paired_wilcox_test(
        by = ARM, variable = AGE, id = USUBJID,
        correct = FALSE, conf.int = TRUE
      ),
    NA
  )

  expect_equal(
    ard_paired_wilcoxtest |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    with(
      data =
        dplyr::full_join(
          ADSL_paired |> dplyr::filter(ARM %in% "Placebo") |> dplyr::rename(ARM1 = ARM, AGE1 = AGE),
          ADSL_paired |> dplyr::filter(ARM %in% "Xanomeline High Dose") |> dplyr::rename(ARM2 = ARM, AGE2 = AGE),
          by = "USUBJID"
        ),
      expr =
        wilcox.test(
          x = AGE1,
          y = AGE2,
          paired = TRUE,
          correct = FALSE,
          conf.int = TRUE
        ) |>
          broom::tidy() |>
          dplyr::select(estimate, conf.low, conf.high) |>
          unclass()
    ),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_equal(
    ADSL_paired |>
      dplyr::mutate(
        ARM = ifelse(dplyr::row_number() == 1L, "3rd ARM", ARM)
      ) |>
      ard_stats_paired_wilcox_test(
        by = ARM, variable = AGE, id = USUBJID,
        correct = FALSE, conf.int = TRUE
      ) |>
      dplyr::pull(error) |>
      getElement(1L),
    "The `by` argument must have two and only two levels."
  )
})
