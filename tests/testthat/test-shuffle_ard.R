skip_if_not(is_pkg_installed("withr"))

test_that("shuffle/trim works", {
  withr::local_options(list(width = 200))
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
    variables = c(AESOC, AEDECOD),
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

  expect_snapshot(ard_shuffled[1:5, ])

  # shuffle & trim
  ard_shuff_trim <- ard_test |>
    shuffle_ard() |>
    as.data.frame()
  expect_snapshot(ard_shuff_trim[1:5, ])
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
  withr::local_options(list(width = 200))
  expect_snapshot(
    ard_continuous(
      ADSL,
      variables = AGEGR1
    ) |>
      shuffle_ard()
  )
})

test_that("shuffle_ard fills missing group levels if the group is meaningful", {
  withr::local_options(list(width = 200))
  # mix of missing/nonmissing group levels present before shuffle
  expect_snapshot(
    bind_ard(
      ard_continuous(ADSL, by = "ARM", variables = "AGE", statistic = ~ continuous_summary_fns("mean")),
      dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p", stat = list(0.05))
    ) |>
      dplyr::filter(dplyr::row_number() <= 5L) |>
      shuffle_ard()
  )

  # no group levels present before shuffle
  expect_snapshot(
    bind_ard(
      ard_continuous(ADSL, variables = "AGE", statistic = ~ continuous_summary_fns("mean")),
      dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p", stat = list(0.05))
    ) |>
      dplyr::filter(dplyr::row_number() <= 5L) |>
      shuffle_ard()
  )

  # mix of group variables - fills overall only if variable has been calculated by group elsewhere
  expect_snapshot(
    bind_ard(
      ard_categorical(ADSL, by = ARM, variables = AGEGR1) |> dplyr::slice(1),
      ard_categorical(ADSL, variables = AGEGR1) |> dplyr::slice(1),
      ard_continuous(ADSL, by = SEX, variables = AGE) |> dplyr::slice(1),
      ard_continuous(ADSL, variables = AGE) |> dplyr::slice(1)
    ) |>
      shuffle_ard() |>
      as.data.frame()
  )

  # mix of hierarchical group variables - fills overall only if variable has been calculated by group elsewhere
  expect_snapshot(
    bind_ard(
      ard_categorical(ADSL, by = c(ARM, SEX), variables = AGEGR1) |> dplyr::slice(1),
      ard_categorical(ADSL, by = SEX, variables = AGEGR1) |> dplyr::slice(1),
      ard_categorical(ADSL, variables = AGEGR1) |> dplyr::slice(1)
    ) |>
      shuffle_ard()
  )

  # fills with a unique group value if one already exists in the df
  adsl_new <- ADSL |>
    dplyr::mutate(ARM = ifelse(ARM == "Placebo", "Overall ARM", ARM))
  expect_snapshot(
    bind_ard(
      ard_continuous(adsl_new, variables = "AGE", statistic = ~ continuous_summary_fns("mean")),
      ard_continuous(adsl_new, by = "ARM", variables = "AGE", statistic = ~ continuous_summary_fns("mean"))
    ) |>
      shuffle_ard()
  )
})

test_that("shuffle_ard doesn't trim off NULL/NA values", {
  # mix of char NA, NULL values
  res <- suppressMessages(
    data.frame(x = rep_len(NA, 10)) |>
      ard_continuous(
        variables = x,
        statistic = ~ continuous_summary_fns(c("median", "p25", "p75"))
      ) |>
      shuffle_ard() |>
      dplyr::pull(stat)
  )

  # check that all rows present
  expect_length(res, 3)
})

test_that("shuffle_ard coerces all factor groups/variables to character", {
  adsl_ <- ADSL |>
    dplyr::mutate(RACE = factor(RACE))

  res <- ard_categorical(
    data = adsl_,
    by = TRT01A,
    variables = c(RACE, ETHNIC)
  ) |>
    shuffle_ard()

  res_classes <- res |>
    dplyr::select(-stat) |>
    sapply(class)

  # all are character
  expect_true(all(res_classes == "character"))

  # correct coersion
  expect_equal(
    sort(unique(res$variable_level)),
    sort(unique(c(as.character(adsl_$RACE), adsl_$ETHNIC)))
  )
})

test_that("shuffle_ard fills missing group levels if the group is meaningful for cardx output", {
  withr::local_options(list(width = 200))
  # cardx ARD: this is a dput() of a cardx result (see commented out code below) SAVED 2024-08-30
  ard_cardx <-
    structure(list(
      group1 = c("ARM", "ARM", "SEX", "SEX"), variable = c(
        "AGEGR1",
        "AGEGR1", "AGEGR1", "AGEGR1"
      ), context = c(
        "stats_chisq_test",
        "stats_chisq_test", "stats_chisq_test", "stats_chisq_test"
      ),
      stat_name = c("statistic", "p.value", "statistic", "p.value"), stat_label = c(
        "X-squared Statistic", "p-value", "X-squared Statistic",
        "p-value"
      ), stat = list(
        statistic = c(`X-squared` = 5.07944166638125),
        p.value = 0.0788884197453486, statistic = c(`X-squared` = 1.03944199945198),
        p.value = 0.594686442507218
      ), fmt_fun = list(
        statistic = 1L,
        p.value = 1L, statistic = 1L, p.value = 1L
      ), warning = list(
        warning = NULL, warning = NULL, warning = NULL, warning = NULL
      ),
      error = list(error = NULL, error = NULL, error = NULL, error = NULL)
    ), row.names = c(
      NA,
      -4L
    ), class = c("card", "tbl_df", "tbl", "data.frame"))

  expect_snapshot(
    ard_cardx |>
      shuffle_ard() |>
      as.data.frame()
  )
})
