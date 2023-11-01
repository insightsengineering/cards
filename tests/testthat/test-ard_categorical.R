test_that("ard_categorical() univariate", {
  expect_error(
    ard_cat_uni <- ard_categorical(mtcars, variables = "am"),
    NA
  )

  expect_equal(
    ard_cat_uni |>
      flatten_ard() |>
      dplyr::filter(stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      as.integer(),
    table(mtcars$am) |> as.integer()
  )

  expect_equal(
    ard_cat_uni |>
      flatten_ard() |>
      dplyr::filter(stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      as.numeric(),
    table(mtcars$am) |> prop.table() |> as.numeric()
  )

  expect_equal(
    ard_cat_uni |> dplyr::filter(stat_name %in% "N") |> dplyr::pull(statistic) |> unlist(),
    sum(!is.na(mtcars$am))
  )

  expect_equal(
    ard_cat_uni |> dplyr::filter(stat_name %in% "N_obs") |> dplyr::pull(statistic) |> unlist(),
    length(mtcars$am)
  )

  expect_equal(
    ard_categorical(
      mtcars,
      variables = starts_with("xxxxx")
    ),
    dplyr::tibble()
  )
})

test_that("ard_categorical() univariate & specified denomiator", {
  expect_error(
    ard_cat_new_denom <-
      ard_categorical(
        mtcars,
        variables = "am",
        denominator = list(mtcars) |> rep_len(100) |> dplyr::bind_rows()
      ),
    NA
  )

  expect_equal(
    ard_cat_new_denom |>
      flatten_ard() |>
      dplyr::filter(stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      as.integer(),
    table(mtcars$am) |> as.integer()
  )

  expect_equal(
    ard_cat_new_denom |>
      flatten_ard() |>
      dplyr::filter(stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      as.numeric(),
      table(mtcars$am) |> prop.table() |> as.numeric() %>% `/`(100)
  )

  expect_equal(
    ard_cat_new_denom |> dplyr::filter(stat_name %in% "N") |> dplyr::pull(statistic) |> unlist(),
    sum(!is.na(mtcars$am)) * 100L
  )
})


test_that("ard_categorical() with strata and by arguments", {
  ADAE_small <-
    ADAE |>
    dplyr::filter(AESOC %in% c("EYE DISORDERS", "INVESTIGATIONS")) |>
    dplyr::slice_head(by = AESOC, n = 3)

  expect_error(
    card_ae_strata <-
      ard_categorical(
        data = ADAE_small,
        strata = c(AESOC, AELLT),
        by = TRTA,
        variables = AESEV,
        denominator = ADSL |> dplyr::rename(TRTA = ARM)
      ),
    NA
  )

  # check that all combinations of AESOC and AELLT are NOT present
  expect_equal(
    card_ae_strata |>
      dplyr::filter(group2_level %in% "EYE DISORDERS",
                    group3_level %in% "NASAL MUCOSA BIOPSY") |>
      nrow(),
    0L
  )

  # check the rate calculations in the first SOC/LLT combination
  expect_equal(
    card_ae_strata |>
      dplyr::filter(
        group1_level %in% "Placebo",
        group2_level %in% "EYE DISORDERS",
        group3_level %in% "EYES SWOLLEN",
        variable_level %in% "MILD",
        stat_name %in% "n"
      ) |>
      dplyr::pull(statistic) |>
      unlist(),
    ADAE_small |>
      dplyr::filter(
        AESOC %in% "EYE DISORDERS",
        AELLT %in% "EYES SWOLLEN",
        TRTA %in% "Placebo",
        AESEV %in% "MILD"
      ) |>
      nrow()
  )

  expect_equal(
    card_ae_strata |>
      dplyr::filter(
        group1_level %in% "Placebo",
        group2_level %in% "EYE DISORDERS",
        group3_level %in% "EYES SWOLLEN",
        variable_level %in% "MILD",
        stat_name %in% "p"
      ) |>
      dplyr::pull(statistic) |>
      unlist(),
    (ADAE_small |>
       dplyr::filter(
         AESOC %in% "EYE DISORDERS",
         AELLT %in% "EYES SWOLLEN",
         TRTA %in% "Placebo",
         AESEV %in% "MILD"
       ) |>
       nrow()) /
      (ADSL |> dplyr::filter(ARM %in% "Placebo") |> nrow())
  )

  expect_equal(
    card_ae_strata |>
      dplyr::filter(
        group1_level %in% "Placebo",
        group2_level %in% "EYE DISORDERS",
        group3_level %in% "EYES SWOLLEN",
        stat_name %in% "N"
      ) |>
      dplyr::pull(statistic) |>
      unlist(),
    ADSL |> dplyr::filter(ARM %in% "Placebo") |> nrow()
  )

})
