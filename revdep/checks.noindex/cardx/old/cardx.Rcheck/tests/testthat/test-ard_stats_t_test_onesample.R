skip_if_not(is_pkg_installed("broom"))

test_that("ard_stats_t_test_onesample() works", {
  # first calculate an object to test against
  expect_silent(
    ard1 <- ard_stats_t_test_onesample(
      cards::ADSL,
      variables = AGE,
      by = ARM,
      conf.level = 0.9,
      mu = 1
    )
  )

  # first check arguments passed and returned correctly
  expect_equal(
    cards::get_ard_statistics(
      ard1,
      group1_level %in% "Placebo"
    )[c("mu", "conf.level")],
    list(mu = 1, conf.level = 0.9)
  )
  # check results are correct
  expect_equal(
    cards::get_ard_statistics(
      ard1,
      group1_level %in% "Placebo"
    )[c("estimate", "conf.low", "conf.high", "p.value")],
    t.test(
      cards::ADSL$AGE[cards::ADSL$ARM == "Placebo"],
      conf.level = 0.9,
      mu = 1
    ) |>
      broom::tidy() |>
      dplyr::select(c("estimate", "conf.low", "conf.high", "p.value")) |>
      as.list()
  )

  # test the structure is good
  expect_silent(cards::check_ard_structure(ard1))

  # empty tibble returned with no variables
  expect_equal(
    ard_stats_t_test_onesample(
      cards::ADSL,
      variables = character(0)
    ),
    dplyr::tibble() |> cards::as_card()
  )
})

test_that("ard_stats_t_test_onesample() follows ard structure", {
  expect_silent(
    ard_stats_t_test_onesample(
      cards::ADSL,
      variables = AGE,
      by = ARM,
      conf.level = 0.9,
      mu = 1
    ) |>
      cards::check_ard_structure()
  )
})
