options("lifecycle_verbosity" = "error")
ADAE_small <-
  ADAE |>
  dplyr::filter(.by = TRTA, dplyr::row_number() <= 2L) |>
  dplyr::select("USUBJID", "TRTA", "AESOC", "AEDECOD", "AESEV") |>
  dplyr::mutate(AESEV = factor(AESEV))

test_that("ard_stack_hierarchical(variables) counts", {
  # ensure that all nested variables appear in resulting ARD
  expect_silent(
    ard <-
      ard_stack_hierarchical(
        ADAE_small,
        variables = c(AESOC, AEDECOD)
      )
  )

  # check the number of rows is expected
  expect_equal(
    nrow(ard),
    length(unique(ADAE_small$AESOC)) + length(unique(ADAE_small$AEDECOD))
  )

  # check AEDECOD match
  expect_equal(
    ard |> dplyr::filter(!is.na(group1)),
    ard_hierarchical_count(ADAE_small, variables = c(AESOC, AEDECOD))
  )

  # check AESOC match
  expect_equal(
    ard |> dplyr::filter(is.na(group1)) |> dplyr::select(-all_ard_group_n(1L)),
    ard_hierarchical_count(ADAE_small, variables = AESOC)
  )
})


test_that("ard_stack_hierarchical(variables) rates", {
  # ensure that all nested variables appear in resulting ARD
  expect_silent(
    ard <-
      ard_stack_hierarchical(
        ADAE_small,
        variables = c(AESOC, AEDECOD),
        id = USUBJID,
        denominator = ADSL
      )
  )

  # check the number of rows is expected
  expect_equal(
    nrow(ard),
    (length(unique(ADAE_small$AESOC)) + length(unique(ADAE_small$AEDECOD))) * 3L # multiply by three for n, N, and p
  )

  # check AEDECOD match
  expect_equal(
    ard |> dplyr::filter(!is.na(group1)),
    ard_hierarchical(
      ADAE_small,
      variables = c(AESOC, AEDECOD),
      id = USUBJID,
      denominator = ADSL
    )
  )

  # check AESOC match
  expect_equal(
    ard |> dplyr::filter(is.na(group1)) |> dplyr::select(-all_ard_group_n(1L)),
    ard_hierarchical(
      ADAE_small |> dplyr::slice_tail(n = 1L, by = c("USUBJID", "TRTA", "AESOC")),
      variables = AESOC,
      id = USUBJID,
      denominator = ADSL
    )
  )
})

test_that("ard_stack_hierarchical(variables) messaging", {
  # missing rows are removed
  expect_snapshot(
    ard <- ADAE_small |>
      dplyr::mutate(AESOC = ifelse(dplyr::row_number() == 1L, NA, AESOC)) |>
      ard_stack_hierarchical(
        variables = c(AESOC, AEDECOD),
        id = USUBJID,
        denominator = ADSL
      )
  )

  # no variables selected
  expect_snapshot(
    error = TRUE,
    ADAE_small |>
      ard_stack_hierarchical(
        variables = starts_with("xxxxx"),
        id = USUBJID,
        denominator = ADSL
      )
  )
})

test_that("ard_stack_hierarchical(by) counts", {
  expect_silent(
    ard <-
      ard_stack_hierarchical(
        ADAE_small,
        variables = c(AESOC, AEDECOD),
        by = TRTA
      )
  )

  # check the number of rows is expected
  expect_equal(
    nrow(ard),
    (length(unique(ADAE_small$AESOC)) + length(unique(ADAE_small$AEDECOD))) * length(unique(ADAE_small$TRTA))
  )

  # check AEDECOD match
  expect_equal(
    ard |> dplyr::filter(!is.na(group2)),
    ard_hierarchical_count(ADAE_small, variables = c(AESOC, AEDECOD), by = TRTA) |>
      cards::tidy_ard_row_order()
  )

  # check AESOC match
  expect_equal(
    ard |> dplyr::filter(is.na(group2)) |> dplyr::select(-all_ard_group_n(2L)),
    ard_hierarchical_count(ADAE_small, variables = AESOC, by = TRTA) |>
      cards::tidy_ard_row_order()
  )
})

test_that("ard_stack_hierarchical(by) rates", {
  # ensure that all nested variables appear in resulting ARD
  expect_silent(
    ard <-
      ard_stack_hierarchical(
        ADAE_small,
        variables = c(AESOC, AEDECOD),
        by = TRTA,
        id = USUBJID,
        denominator = ADSL
      )
  )

  # check the number of rows is expected
  expect_equal(
    nrow(ard),
    ((length(unique(ADAE_small$AESOC)) + length(unique(ADAE_small$AEDECOD)))) * length(unique(ADAE_small$TRTA)) * 3L # multiply by three for n, N, and p
  )

  # check AEDECOD match
  expect_equal(
    ard |> dplyr::filter(!is.na(group2)),
    ard_hierarchical(
      ADAE_small,
      variables = c(AESOC, AEDECOD),
      by = TRTA,
      id = USUBJID,
      denominator = ADSL
    ) |>
      cards::tidy_ard_row_order()
  )

  # check AESOC match
  expect_equal(
    ard |> dplyr::filter(is.na(group2)) |> dplyr::select(-all_ard_group_n(2L)),
    ard_hierarchical(
      ADAE_small |> dplyr::slice_tail(n = 1L, by = c("USUBJID", "TRTA", "AESOC")),
      variables = AESOC,
      by = TRTA,
      id = USUBJID,
      denominator = ADSL
    ) |>
      cards::tidy_ard_row_order()
  )
})

test_that("ard_stack_hierarchical(by) messaging", {
  # missing rows are removed
  expect_snapshot(
    ard <- ADAE_small |>
      dplyr::mutate(TRTA = ifelse(dplyr::row_number() == 1L, NA, TRTA)) |>
      ard_stack_hierarchical(
        variables = c(AESOC, AEDECOD),
        by = TRTA,
        id = USUBJID,
        denominator = ADSL
      )
  )
})

test_that("ard_stack_hierarchical(denominator) messaging", {
  # when the wrong type is passed to the argument
  expect_snapshot(
    error = TRUE,
    ADAE_small |>
      ard_stack_hierarchical(
        variables = c(AESOC, AEDECOD),
        by = TRTA,
        denominator = letters
      )
  )
})

test_that("ard_stack_hierarchical(denominator) univariate tabulations", {
  # test that we get the expected univariate by variable tabulations
  expect_equal(
    ADAE_small |>
      ard_stack_hierarchical(
        variables = c(AESOC, AEDECOD),
        by = TRTA,
        denominator = ADSL |> dplyr::rename(TRTA = TRT01A)
      ) |>
      dplyr::filter(variable == "TRTA") |>
      dplyr::select(-all_missing_columns()),
    ard_categorical(ADSL |> dplyr::rename(TRTA = TRT01A), variables = TRTA) |>
      dplyr::select(-all_missing_columns())
  )


  # everything still works when the by variable includes vars not in the denom data frame
  expect_equal(
    ard <- ADAE_small |>
      ard_stack_hierarchical(
        variables = c(AESOC, AEDECOD),
        by = c(TRTA, AESEV),
        denominator = ADSL |> dplyr::rename(TRTA = TRT01A)
      ) |>
      dplyr::filter(variable == "TRTA") |>
      dplyr::select(-all_missing_columns()),
    ard_categorical(ADSL |> dplyr::rename(TRTA = TRT01A), variables = TRTA) |>
      dplyr::select(-all_missing_columns())
  )
  expect_true(nrow(dplyr::filter(ard, variable == "AESEV")) == 0L)
})

test_that("ard_stack_hierarchical(denominator,total_n)", {
  # check N is correct when denom is a data frame
  expect_equal(
    ADAE_small |>
      ard_stack_hierarchical(
        variables = c(AESOC, AEDECOD),
        denominator = ADSL,
        total_n = TRUE
      ) |>
      dplyr::filter(variable == "..ard_total_n..") |>
      dplyr::select(-all_missing_columns()),
    ard_total_n(ADSL) |>
      dplyr::select(-all_missing_columns())
  )

  # check N is correct when denom is an integer
  expect_equal(
    ADAE_small |>
      ard_stack_hierarchical(
        variables = c(AESOC, AEDECOD),
        denominator = nrow(ADSL),
        total_n = TRUE
      ) |>
      dplyr::filter(variable == "..ard_total_n..") |>
      dplyr::select(-all_missing_columns()),
    ard_total_n(ADSL) |>
      dplyr::select(-all_missing_columns())
  )
})

test_that("ard_stack_hierarchical(denominator,total_n) messaging", {
  # requesting total N without a denominator
  expect_snapshot(
    ard <- ADAE_small |>
      ard_stack_hierarchical(
        variables = c(AESOC, AEDECOD),
        total_n = TRUE
      )
  )
})

test_that("ard_stack_hierarchical(id,denominator) messaging", {
  # the denominator must be specified when using the `id` argument
  expect_snapshot(
    error = TRUE,
    ard_stack_hierarchical(
      ADAE,
      variables = c(AESOC, AEDECOD),
      by = TRTA,
      id = USUBJID
    )
  )
})


