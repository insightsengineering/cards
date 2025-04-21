test_that("ard_categorical_max() works with default settings", {
  withr::local_options(list(width = 200))

  expect_message(
    res <- ard_categorical_max(
      cards::ADAE,
      variables = AESEV,
      id = USUBJID,
      by = TRTA
    )
  )
  expect_snapshot(res |> print(n = 20, columns = "all"))

  expect_equal(
    res |>
      dplyr::filter(
        group1_level == "Placebo",
        variable_level == "SEVERE",
        stat_name == "n"
      ) |>
      cards::get_ard_statistics(),
    list(
      n = cards::ADAE |>
        dplyr::filter(
          TRTA == "Placebo",
          AESEV == "SEVERE"
        ) |>
        dplyr::slice_tail(n = 1L, by = all_of(c("USUBJID", "TRTA", "AESEV"))) |>
        nrow()
    )
  )

  # with denominator
  expect_snapshot(
    ard_categorical_max(
      cards::ADAE |> dplyr::group_by(TRTA),
      variables = AESEV,
      id = USUBJID,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM)
    ) |>
      print(n = 20, columns = "all")
  )

  # with multiple variables
  expect_message(expect_message(
    res2 <- ard_categorical_max(
      cards::ADAE,
      variables = c(AESEV, AESER),
      id = USUBJID,
      by = TRTA
    )
  ))
  expect_equal(unique(res2$variable), c("AESEV", "AESER"))
  expect_equal(
    res,
    res2 |> dplyr::filter(variable == "AESEV")
  )
})

test_that("ard_categorical_max(statistic) works", {
  withr::local_options(list(width = 200))

  expect_snapshot(
    ard_categorical_max(
      cards::ADAE,
      variables = AESEV,
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      statistic = ~"n"
    )
  )
})

test_that("ard_categorical_max(denominator) works", {
  withr::local_options(list(width = 200))

  # default denominator
  expect_snapshot(
    ard_categorical_max(
      cards::ADAE,
      variables = AESEV,
      id = USUBJID,
      by = TRTA
    )
  )

  # numeric denominator
  expect_snapshot(
    ard_categorical_max(
      cards::ADAE,
      variables = AESEV,
      id = USUBJID,
      by = TRTA,
      denominator = 100
    )
  )
})

test_that("ard_categorical_max(quiet) works", {
  withr::local_options(list(width = 200))

  expect_silent(
    ard_categorical_max(
      cards::ADAE,
      variables = c(AESER, AESEV),
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      quiet = TRUE
    )
  )
})

test_that("ard_categorical_max() works with pre-ordered factor variables", {
  withr::local_options(list(width = 200))

  # ordered factor variable
  adae <- cards::ADAE |>
    dplyr::mutate(AESEV = factor(cards::ADAE$AESEV, ordered = TRUE))
  # unordered factor variable
  adae_unord <- cards::ADAE |>
    dplyr::mutate(AESEV = factor(cards::ADAE$AESEV, ordered = FALSE))

  expect_message(
    res <- ard_categorical_max(
      adae_unord,
      variables = AESEV,
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      ordered = TRUE
    )
  )
  expect_snapshot(res |> print(n = 20, columns = "all"))

  expect_equal(
    res |>
      dplyr::mutate(variable_level = as.character(unlist(variable_level))) |>
      dplyr::filter(
        group1_level == "Placebo",
        variable_level == "MODERATE",
        stat_name == "n"
      ) |>
      cards::get_ard_statistics(),
    list(
      n = adae |>
        dplyr::arrange(AESEV) |>
        dplyr::slice_tail(n = 1L, by = all_of(c("USUBJID", "TRTA"))) |>
        dplyr::filter(
          TRTA == "Placebo",
          AESEV == "MODERATE"
        ) |>
        nrow()
    )
  )

  expect_message(
    res_unord <- ard_categorical_max(
      adae_unord,
      variables = AESEV,
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM)
    )
  )
  expect_equal(res$stat[[1]], res_unord$stat[[1]])

  expect_message(
    res2 <- ard_categorical_max(
      adae,
      variables = AESEV,
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM)
    )
  )
  expect_equal(res, res2, ignore_attr = "class")

  # multiple variables
  expect_message(expect_message(
    res3 <- ard_categorical_max(
      adae,
      variables = c(SEX, AESEV),
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      ordered = c(FALSE, TRUE)
    )
  ))
  expect_equal(
    res,
    res3 |> dplyr::filter(variable == "AESEV")
  )

  # named vector
  expect_message(expect_message(
    res4 <- ard_categorical_max(
      adae,
      variables = c(SEX, AESEV),
      id = USUBJID,
      by = TRTA,
      denominator = cards::ADSL |> dplyr::rename(TRTA = ARM),
      ordered = c(AESEV = TRUE, SEX = FALSE)
    )
  ))
  expect_equal(res3, res4)
})

test_that("ard_categorical_max() errors with incomplete factor columns", {
  # Check error when factors have no levels
  expect_snapshot(
    error = TRUE,
    ard_categorical_max(
      cards::ADAE |>
        dplyr::mutate(AESOC = factor(AESOC, levels = character(0))),
      variables = AESOC,
      id = USUBJID,
      by = TRTA
    )
  )

  # Check error when factor has NA level
  expect_snapshot(
    error = TRUE,
    ard_categorical_max(
      cards::ADAE |>
        dplyr::mutate(SEX = factor(SEX, levels = c("F", "M", NA), exclude = NULL)),
      variables = SEX,
      id = USUBJID,
      by = TRTA
    )
  )
})

test_that("ard_categorical_max() works without any variables", {
  expect_snapshot(
    ard_categorical_max(
      data = cards::ADAE,
      variables = starts_with("xxxx"),
      id = USUBJID,
      by = c(TRTA, AESEV)
    )
  )
})

test_that("ard_categorical_max() follows ard structure", {
  expect_message(
    ard_categorical_max(
      cards::ADAE,
      variables = AESOC,
      id = USUBJID
    ) |>
      cards::check_ard_structure(method = FALSE)
  )
})
