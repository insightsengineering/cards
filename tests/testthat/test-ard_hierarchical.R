# ard_hierarchical() -----------------------------------------------------------
test_that("ard_hierarchical() works without by variables", {
  expect_error(
    ard_heir_no_by <-
      ard_hierarchical(
        data = ADAE,
        variables = c(AESOC, AETERM),
        denominator = ADSL
      ),
    NA
  )
  expect_snapshot(class(ard_heir_no_by))

  expect_equal(
    ard_heir_no_by |>
      dplyr::filter(group1_level == "CARDIAC DISORDERS", variable_level == "ATRIAL FIBRILLATION") |>
      get_ard_statistics(.attributes = NULL),
    dplyr::tibble(
      n =
        ADAE |>
        dplyr::filter(AESOC == "CARDIAC DISORDERS", AETERM == "ATRIAL FIBRILLATION") |>
        nrow(),
      p = n / nrow(ADSL)
    ) |>
      as.list()
  )
})

test_that("ard_hierarchical() works with by variable", {
  expect_error(
    ard_heir_with_by <-
      ard_hierarchical(
        data = ADAE,
        variables = c(AESOC, AETERM),
        by = TRTA,
        denominator = ADSL |> dplyr::rename(TRTA = ARM)
      ),
    NA
  )

  expect_equal(
    ard_heir_with_by |>
      dplyr::filter(group1_level == "Placebo", group2_level == "CARDIAC DISORDERS", variable_level == "ATRIAL FIBRILLATION") |>
      get_ard_statistics(.attributes = NULL),
    dplyr::tibble(
      n =
        ADAE |>
        dplyr::filter(TRTA == "Placebo", AESOC == "CARDIAC DISORDERS", AETERM == "ATRIAL FIBRILLATION") |>
        nrow(),
      p = n / (ADSL |> dplyr::filter(ARM == "Placebo") |> nrow())
    ) |>
      as.list()
  )
})

test_that("ard_hierarchical() works with by variable not present in 'denominator'", {
  expect_error(
    ard_heir_with_by <-
      ard_hierarchical(
        data = ADAE,
        variables = c(AESOC, AETERM),
        by = c(TRTA, AESEV),
        denominator = ADSL |> dplyr::rename(TRTA = ARM)
      ),
    NA
  )

  expect_equal(
    ard_heir_with_by |>
      dplyr::filter(group1_level == "Placebo",
                    group2_level == "MILD",
                    group3_level == "CARDIAC DISORDERS",
                    variable_level == "ATRIAL HYPERTROPHY") |>
      get_ard_statistics(.attributes = NULL),
    dplyr::tibble(
      n =
        ADAE |>
        dplyr::filter(TRTA == "Placebo",
                      AESEV == "MILD",
                      AESOC == "CARDIAC DISORDERS",
                      AETERM == "ATRIAL HYPERTROPHY") |>
        nrow(),
      p = n / (ADSL |> dplyr::filter(ARM == "Placebo") |> nrow())
    ) |>
      as.list()
  )
})


# ard_hierarchical_count() -----------------------------------------------------
test_that("ard_hierarchical_count() works without by variables", {
  expect_error(
    ard_heir_no_by <-
      ard_hierarchical_count(
        data = ADAE,
        variables = c(AESOC, AETERM)
      ),
    NA
  )
  expect_snapshot(class(ard_heir_no_by))

  expect_equal(
    ard_heir_no_by |>
      dplyr::filter(group1_level == "CARDIAC DISORDERS", variable_level == "ATRIAL FIBRILLATION") |>
      get_ard_statistics(.attributes = NULL),
    list(
      n =
        ADAE |>
        dplyr::filter(AESOC == "CARDIAC DISORDERS", AETERM == "ATRIAL FIBRILLATION") |>
        nrow()
    )
  )

  expect_equal(
    ard_heir_no_by |>
      dplyr::filter(variable == "AESOC", variable_level == "CARDIAC DISORDERS") |>
      get_ard_statistics(.attributes = NULL),
    list(
      n =
        ADAE |>
        dplyr::filter(AESOC == "CARDIAC DISORDERS") |>
        nrow()
    )
  )
})

test_that("ard_hierarchical_count() works with by variable", {
  expect_error(
    ard_heir_with_by <-
      ard_hierarchical_count(
        data = ADAE,
        variables = c(AESOC, AETERM),
        by = TRTA
      ),
    NA
  )

  expect_equal(
    ard_heir_with_by |>
      dplyr::filter(group1_level == "Placebo",
                    group2_level == "CARDIAC DISORDERS",
                    variable_level == "ATRIAL HYPERTROPHY") |>
      get_ard_statistics(.attributes = NULL),
    list(
      n =
        ADAE |>
        dplyr::filter(TRTA == "Placebo", AESOC == "CARDIAC DISORDERS", AETERM == "ATRIAL HYPERTROPHY") |>
        nrow()
    )
  )
})

test_that("ard_hierarchical_count() works with by variable not present in 'denominator'", {
  expect_error(
    ard_heir_with_by <-
      ard_hierarchical_count(
        data = ADAE,
        variables = c(AESOC, AETERM),
        by = c(TRTA, AESEV)
      ),
    NA
  )

  expect_equal(
    ard_heir_with_by |>
      dplyr::filter(group1_level == "Placebo",
                    group2_level == "MILD",
                    group3_level == "CARDIAC DISORDERS",
                    variable_level == "ATRIAL HYPERTROPHY") |>
      get_ard_statistics(.attributes = NULL),
    list(
      n =
        ADAE |>
        dplyr::filter(TRTA == "Placebo",
                      AESEV == "MILD",
                      AESOC == "CARDIAC DISORDERS",
                      AETERM == "ATRIAL HYPERTROPHY") |>
        nrow()
    )
  )
})
