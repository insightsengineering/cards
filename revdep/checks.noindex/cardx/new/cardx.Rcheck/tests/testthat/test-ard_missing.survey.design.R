skip_if_not(is_pkg_installed("survey", reference_pkg = "cardx"))

# Test survey.design working (2x3)
test_that("ard_missing.survey.design() works", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  df_titanic <- as.data.frame(Titanic) |> tidyr::uncount(weights = Freq)


  # without by
  expect_error(
    ard_svy_missing <- ard_missing(svy_titanic, variables = c(Class, Age), by = NULL),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_missing, method = FALSE))

  # without by, single var
  expect_error(
    ard_svy_missing <- ard_missing(svy_titanic, variables = Class, by = NULL),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_missing, method = FALSE))

  # with by, single var
  expect_error(
    ard_svy_missing <- ard_missing(svy_titanic, variables = Class, by = Survived),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_missing, method = FALSE))

  # with by
  expect_error(
    ard_svy_missing <- ard_missing(svy_titanic, variables = c(Class, Age), by = Survived),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_missing, method = FALSE))


  # check the calculated stats are correct
  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "N_obs") |> unlist(),
    ard_missing(df_titanic, variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N_obs") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "N_nonmiss") |> unlist(),
    ard_missing(df_titanic, variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N_nonmiss") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "N_nonmiss") |> unlist(),
    ard_missing(df_titanic, variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N_nonmiss") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "p_nonmiss") |> unlist(),
    ard_missing(df_titanic, variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "p_nonmiss") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "N_miss") |> unlist(),
    ard_missing(df_titanic, variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N_miss") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "p_miss") |> unlist(),
    ard_missing(df_titanic, variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "p_miss") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "N_miss_unweighted") |> unlist() |> unname(),
    ard_missing(as.data.frame(Titanic), variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N_miss") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "N_obs_unweighted") |> unlist() |> unname(),
    ard_missing(as.data.frame(Titanic), variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N_obs") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "p_miss_unweighted") |> unlist() |> unname(),
    ard_missing(as.data.frame(Titanic), variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "p_miss") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "N_nonmiss_unweighted") |> unlist() |> unname(),
    ard_missing(as.data.frame(Titanic), variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N_nonmiss") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_missing |> dplyr::arrange_all(), stat_name %in% "p_nonmiss_unweighted") |> unlist() |> unname(),
    ard_missing(as.data.frame(Titanic), variables = c(Class, Age), by = Survived) |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "p_nonmiss") |> unlist() |> unname()
  )
})

test_that("ard_missing.survey.design() follows ard structure", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  expect_silent(
    ard_missing(svy_titanic, variables = c(Class, Age), by = NULL) |>
      cards::check_ard_structure(method = FALSE)
  )
})
