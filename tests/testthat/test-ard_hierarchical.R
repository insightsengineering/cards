# ard_hierarchical() -----------------------------------------------------------
test_that("ard_hierarchical() works without by variables", {
  expect_error(
    ard_heir_no_by <-
      ard_hierarchical(
        data = ADAE,
        variables = c(AESOC, AEDECOD),
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
          dplyr::filter(AESOC == "CARDIAC DISORDERS", AEDECOD == "ATRIAL FIBRILLATION") |>
          nrow(),
      N = nrow(ADSL),
      p = n / N
    ) |>
      as.list()
  )
})

test_that("ard_hierarchical() works with by variable", {
  expect_error(
    ard_heir_with_by <-
      ard_hierarchical(
        data = ADAE,
        variables = c(AESOC, AEDECOD),
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
          dplyr::filter(TRTA == "Placebo", AESOC == "CARDIAC DISORDERS", AEDECOD == "ATRIAL FIBRILLATION") |>
          nrow(),
      N = ADSL |> dplyr::filter(ARM == "Placebo") |> nrow(),
      p = n / N
    ) |>
      as.list()
  )
})

test_that("ard_hierarchical() works with by variable not present in 'denominator'", {
  expect_error(
    ard_heir_with_by <-
      ard_hierarchical(
        data = ADAE,
        variables = c(AESOC, AEDECOD),
        by = c(TRTA, AESEV),
        denominator = ADSL |> dplyr::rename(TRTA = ARM)
      ),
    NA
  )

  expect_equal(
    ard_heir_with_by |>
      dplyr::filter(
        group1_level == "Placebo",
        group2_level == "MILD",
        group3_level == "CARDIAC DISORDERS",
        variable_level == "ATRIAL HYPERTROPHY"
      ) |>
      get_ard_statistics(.attributes = NULL),
    dplyr::tibble(
      n =
        ADAE |>
          dplyr::filter(
            TRTA == "Placebo",
            AESEV == "MILD",
            AESOC == "CARDIAC DISORDERS",
            AEDECOD == "ATRIAL HYPERTROPHY"
          ) |>
          nrow(),
      N = ADSL |> dplyr::filter(ARM == "Placebo") |> nrow(),
      p = n / N
    ) |>
      as.list()
  )
})

test_that("ard_hierarchical() works without any variables", {
  expect_snapshot(
    ard_hierarchical(
      data = ADAE,
      variables = starts_with("xxxx"),
      by = c(TRTA, AESEV)
    )
  )
})

test_that("ard_hierarchical(id) argument works", {
  expect_snapshot(
    ard_hierarchical(
      data = ADAE,
      variables = c(AESOC, AEDECOD),
      by = c(TRTA, AESEV),
      denominator = ADSL |> dplyr::rename(TRTA = ARM),
      id = USUBJID
    ) |>
      head(1L)
  )

  # testing pluralization works in warning message
  expect_snapshot(
    ard_hierarchical(
      data = ADAE,
      variables = c(AESOC, AEDECOD),
      by = c(TRTA, AESEV),
      denominator = ADSL |> dplyr::rename(TRTA = ARM),
      id = c(USUBJID, SITEID)
    ) |>
      head(1L)
  )
})

# ard_hierarchical_count() -----------------------------------------------------
test_that("ard_hierarchical_count() works without by variables", {
  expect_error(
    ard_heir_no_by <-
      ard_hierarchical_count(
        data = ADAE,
        variables = c(AESOC, AEDECOD)
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
          dplyr::filter(AESOC == "CARDIAC DISORDERS", AEDECOD == "ATRIAL FIBRILLATION") |>
          nrow()
    )
  )

  expect_equal(
    ard_hierarchical_count(
      data = ADAE,
      variables = AESOC
    ) |>
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
        variables = c(AESOC, AEDECOD),
        by = TRTA
      ),
    NA
  )

  expect_equal(
    ard_heir_with_by |>
      dplyr::filter(
        group1_level == "Placebo",
        group2_level == "CARDIAC DISORDERS",
        variable_level == "ATRIAL HYPERTROPHY"
      ) |>
      get_ard_statistics(.attributes = NULL),
    list(
      n =
        ADAE |>
          dplyr::filter(TRTA == "Placebo", AESOC == "CARDIAC DISORDERS", AEDECOD == "ATRIAL HYPERTROPHY") |>
          nrow()
    )
  )
})

test_that("ard_hierarchical_count() works with by variable not present in 'denominator'", {
  expect_error(
    ard_heir_with_by <-
      ard_hierarchical_count(
        data = ADAE,
        variables = c(AESOC, AEDECOD),
        by = c(TRTA, AESEV)
      ),
    NA
  )

  expect_equal(
    ard_heir_with_by |>
      dplyr::filter(
        group1_level == "Placebo",
        group2_level == "MILD",
        group3_level == "CARDIAC DISORDERS",
        variable_level == "ATRIAL HYPERTROPHY"
      ) |>
      get_ard_statistics(.attributes = NULL),
    list(
      n =
        ADAE |>
          dplyr::filter(
            TRTA == "Placebo",
            AESEV == "MILD",
            AESOC == "CARDIAC DISORDERS",
            AEDECOD == "ATRIAL HYPERTROPHY"
          ) |>
          nrow()
    )
  )
})

test_that("ard_hierarchical_count() works without any variables", {
  expect_snapshot(
    ard_hierarchical_count(
      data = ADAE,
      variables = starts_with("xxxx"),
      by = c(TRTA, AESEV)
    )
  )
})


test_that("ard_hierarchical() and ard_hierarchical_count() with grouped data works", {
  expect_equal(
    ADAE |>
      dplyr::group_by(TRTA) |>
      ard_hierarchical(
        variables = c(AESOC, AEDECOD),
        denominator = ADSL |> dplyr::rename(TRTA = ARM)
      ),
    ard_hierarchical(
      data = ADAE,
      by = TRTA,
      variables = c(AESOC, AEDECOD),
      denominator = ADSL |> dplyr::rename(TRTA = ARM)
    )
  )

  expect_equal(
    ADAE |>
      dplyr::group_by(TRTA) |>
      ard_hierarchical_count(
        variables = c(AESOC, AEDECOD)
      ),
    ard_hierarchical_count(
      data = ADAE,
      by = TRTA,
      variables = c(AESOC, AEDECOD)
    )
  )
})

test_that("ard_hierarchical() follows ard structure", {
  expect_silent(
    ADAE |>
      dplyr::group_by(TRTA) |>
      ard_hierarchical_count(
        variables = c(AESOC, AETERM)
      ) |>
      check_ard_structure(method = FALSE)
  )
})

test_that("ard_hierarchical() errors with incomplete factor columns", {
  # Check error when factors have no levels
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = character(0))) |>
      ard_hierarchical(
        variables = c(vs, am)
      )
  )

  # Check error when factor has NA level
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = c(0, 1, NA), exclude = NULL)) |>
      ard_hierarchical(
        variables = c(vs, am)
      )
  )
})


test_that("ard_hierarchical_count() errors with incomplete factor columns", {
  # Check error when factors have no levels
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = character(0))) |>
      ard_hierarchical_count(
        variables = c(vs, am)
      )
  )

  # Check error when factor has NA level
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = c(0, 1, NA), exclude = NULL)) |>
      ard_hierarchical_count(
        variables = c(vs, am)
      )
  )
})
