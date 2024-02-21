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
  expect_type(ard_shuff_trim$stat, "double")
  # no list columns
  expect_true(!any(map_lgl(ard_shuff_trim, is.list)))
})

test_that("shuffle_ard handles protected names", {
  ard_test <- ard_categorical(
    ADSL |> dplyr::rename(stat = ARM),
    by = "stat",
    variables = "AGEGR1"
  ) |>
    shuffle_ard()

  expect_equal(names(ard_test)[1], "stat.1")
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
      ard_continuous(ADSL, by = "ARM", variables = "AGE", statistic = ~ continuous_summary_fns("mean")),
      dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p", stat = list(0.05))
    ) |>
      shuffle_ard()
  )

  # no group levels present before shuffle
  expect_snapshot(
    bind_ard(
      ard_continuous(ADSL, variables = "AGE", statistic = ~ continuous_summary_fns("mean")),
      dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p", stat = list(0.05))
    ) |>
      shuffle_ard()
  )
})

test_that("shuffle_ard doesn't trim off NULL/NA values", {
  # mix of char NA, NULL values
  res <- suppressMessages(
    data.frame(x = rep_len(NA_character_, 10)) |>
      ard_continuous(
        variables = x,
        statistic = ~ continuous_summary_fns(c("median", "p25", "p75"))
      ) |>
      shuffle_ard() |>
      dplyr::pull(stat)
  )

  # check that all rows present and result is a numeric vector
  expect_length(res, 3)
  expect_equal(class(res), "numeric")
})

test_that("shuffle_ard trims statistics with length > 1", {
  expect_equal(
    ard_continuous(
      ADSL,
      variables = AGE,
      statistic = list(AGE = list(
        long_result = \(x) 1:3,
        long_list_result = \(x) list(1:3)
      ))
    ) |>
      shuffle_ard() |>
      nrow(),
    0L
  )
})
