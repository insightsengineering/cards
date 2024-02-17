test_that("shuffle/trim works", {
  # shuffle without group/var levels
  ard_simple <- ard_continuous(ADSL, variables = "AGE")

  ard_simple_shuffled <- ard_simple |>
    shuffle_ard(trim = FALSE) |>
    as.data.frame()

  expect_snapshot(ard_simple_shuffled)

  # shuffle back-fills groupings
  ard_grp <- bind_ard(
    ard_categorical(ADSL, variables = "ARM"),
    ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
  )
  ard_grp_shuffled <- ard_grp |>
    shuffle_ard(trim = FALSE) |>
    dplyr::filter(!stat_name == "N")
  expect_true(all(!is.na(ard_grp_shuffled$ARM)))

  ard_hier <- ard_hierarchical_count(
    data = ADAE,
    variables = c(AESOC, AETERM),
    by = TRTA
  )
  ard_hier_shuff <- ard_hier |>
    shuffle_ard(trim = FALSE) |>
    as.data.frame()
  expect_true(all(!is.na(ard_hier_shuff$AESOC)))


  # shuffle many different formats
  ard_test <- bind_ard(
    ard_categorical(ADSL, variables = "ARM"),
    ard_continuous(ADSL, by = "ARM", variables = "AGE", stat_label = ~ list(c("mean", "sd") ~ "Mean(SD)")),
    ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"),
    ard_missing(ADSL, by = "ARM", variables = c("AGEGR1", "AGE"))
  )
  ard_shuffled <- ard_test |>
    shuffle_ard() |>
    as.data.frame()

  expect_snapshot(ard_shuffled)

  # shuffle & trim
  ard_shuff_trim <- ard_test |>
    shuffle_ard() |>
    as.data.frame()
  expect_snapshot(ard_shuff_trim)
  # only numeric stats
  expect_type(ard_shuff_trim$statistic, "double")
  # no list columns
  expect_true(!any(map_lgl(ard_shuff_trim, is.list)))
})

test_that("shuffle_ard handles protected names", {
  ard_test <- ard_categorical(
    ADSL |> dplyr::rename(statistic = ARM),
    by = "statistic",
    variables = "AGEGR1"
  ) |>
    shuffle_ard()

  expect_equal(names(ard_test)[1], "statistic.1")
})

test_that("shuffle_ard notifies user about warnings/errors before dropping", {
  expect_snapshot(
    ard_continuous(
      ADSL,
      variables = AGEGR1
    ) |>
      shuffle_ard()
  )
})

test_that("shuffle_ard fills missing group levels if the group is meaningful", {
  # mix of missing/nonmissing group levels present before shuffle
  expect_snapshot(
    bind_ard(
      ard_continuous(ADSL, by = "ARM", variables = "AGE", statistics = ~ continuous_variable_summary_fns("mean")),
      dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p", statistic = list(0.05))
    ) |>
      shuffle_ard()
  )

  # no group levels present before shuffle
  expect_snapshot(
    bind_ard(
      ard_continuous(ADSL, variables = "AGE", statistics = ~ continuous_variable_summary_fns("mean")),
      dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p", statistic = list(0.05))
    ) |>
      shuffle_ard()
  )
})