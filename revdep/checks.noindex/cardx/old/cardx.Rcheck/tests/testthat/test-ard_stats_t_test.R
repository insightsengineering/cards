skip_if_not(is_pkg_installed("broom"))

test_that("ard_stats_t_test() works", {
  # One Sample t-test works
  expect_error(
    ard_single <- ard_stats_t_test(cards::ADSL, variable = AGE, var.equal = TRUE),
    NA
  )

  expect_equal(
    ard_single |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    t.test(
      cards::ADSL$AGE,
      var.equal = TRUE
    ) |>
      broom::tidy() |>
      dplyr::select(estimate, conf.low, conf.high) |>
      unclass(),
    ignore_attr = TRUE
  )

  # Two Sample t-test works
  expect_error(
    ard_ttest <-
      cards::ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_stats_t_test(by = ARM, variable = AGE, var.equal = TRUE),
    NA
  )

  expect_equal(
    ard_ttest |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    t.test(
      AGE ~ ARM,
      data = cards::ADSL |> dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")),
      var.equal = TRUE
    ) |>
      broom::tidy() |>
      dplyr::select(estimate, conf.low, conf.high) |>
      unclass(),
    ignore_attr = TRUE
  )

  # errors are properly handled
  expect_equal(
    cards::ADSL |>
      ard_stats_t_test(by = ARM, variable = AGE, var.equal = TRUE) |>
      dplyr::select(error) %>%
      is.null(),
    FALSE
  )

  # test that the function works with multiple variables at once
  expect_equal(
    dplyr::bind_rows(
      ard_ttest,
      cards::ADSL |>
        dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
        ard_stats_t_test(by = ARM, variable = BMIBL, var.equal = TRUE)
    ),
    cards::ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_stats_t_test(by = ARM, variable = c(AGE, BMIBL), var.equal = TRUE)
  )
})

test_that("ard_stats_paired_t_test() works", {
  ADSL_paired <-
    cards::ADSL[c("ARM", "AGE")] |>
    dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
    dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number())

  expect_error(
    ard_paired_ttest <-
      ADSL_paired |>
      ard_stats_paired_t_test(by = ARM, variable = AGE, id = USUBJID, var.equal = TRUE),
    NA
  )

  expect_equal(
    ard_paired_ttest |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "conf.low", "conf.high")),
    with(
      data =
        dplyr::full_join(
          ADSL_paired |> dplyr::filter(ARM %in% "Placebo") |> dplyr::rename(ARM1 = ARM, AGE1 = AGE),
          ADSL_paired |> dplyr::filter(ARM %in% "Xanomeline High Dose") |> dplyr::rename(ARM2 = ARM, AGE2 = AGE),
          by = "USUBJID"
        ),
      expr =
        t.test(
          x = AGE1,
          y = AGE2,
          paired = TRUE,
          var.equal = TRUE
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
      ard_stats_paired_t_test(by = ARM, variable = AGE, id = USUBJID, var.equal = TRUE) |>
      dplyr::select(error) %>%
      is.null(),
    FALSE
  )
})

test_that("ard_stats_t_test() follows ard structure", {
  expect_silent(
    cards::ADSL |>
      dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
      ard_stats_t_test(by = ARM, variable = AGE, var.equal = TRUE) |>
      cards::check_ard_structure()
  )
})
