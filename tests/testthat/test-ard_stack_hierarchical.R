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
    (length(unique(ADAE_small$AESOC)) + length(unique(ADAE_small$AEDECOD))) * 3L # multply by three for n, N, and p
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
