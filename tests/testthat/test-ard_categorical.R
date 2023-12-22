test_that("ard_categorical() univariate", {
  expect_error(
    ard_cat_uni <- ard_categorical(mtcars, variables = "am"),
    NA
  )
  expect_snapshot(class(ard_cat_uni))

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
    dplyr::filter(ard_cat_uni, stat_name %in% "N")$statistic[[1]],
    sum(!is.na(mtcars$am))
  )

  expect_equal(
    ard_categorical(
      mtcars,
      variables = starts_with("xxxxx")
    ),
    dplyr::tibble()
  )

  # works for ordered factors
  expect_equal(
    ard_categorical(
      mtcars |> dplyr::mutate(cyl = factor(cyl, ordered = TRUE)),
      variables = cyl
    ) |>
      dplyr::select(stat_name, stat_label, statistic),
    ard_categorical(
      mtcars |> dplyr::mutate(cyl = factor(cyl, ordered = FALSE)),
      variables = cyl
    ) |>
      dplyr::select(stat_name, stat_label, statistic)
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
  expect_snapshot(class(ard_cat_new_denom))


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
    dplyr::filter(ard_cat_new_denom, stat_name %in% "N")$statistic[[1]],
    sum(!is.na(mtcars$am)) * 100L
  )
})

test_that("ard_continuous(fmt_fn) argument works", {
  ard_categorical(
    mtcars,
    variables = "am",
    fmt_fn =
      list(
        am =
          list(
            p = function(x) round5(x * 100, digits = 3) |> as.character(),
            N = function(x) format(round5(x, digits = 2), nsmall = 2),
            N_obs = function(x) format(round5(x, digits = 2), nsmall = 2)
          )
      )
  ) |>
    apply_statistic_fmt_fn() |>
    dplyr::select(variable, variable_level, stat_name, statistic, statistic_fmt) |>
    as.data.frame() |>
    expect_snapshot()

  ard_categorical(
    mtcars,
    variables = c("am","vs"),
    fmt_fn = list(
      am = list(p = function(x) round5(x * 100, digits = 3)),
      vs = list(p = function(x) round5(x * 100, digits = 1))
      )
    )|>
    apply_statistic_fmt_fn() |>
    dplyr::select(variable, variable_level, stat_name, statistic, statistic_fmt) |>
    as.data.frame() |>
    expect_snapshot()

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
      getElement(1),
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
      getElement(1),
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
        stat_name %in% "N"
      ) |>
      dplyr::pull(statistic) |>
      getElement(1),
    ADSL |> dplyr::filter(ARM %in% "Placebo") |> nrow()
  )

})

test_that("ard_categorical(stat_labels) argument works", {
  # formula
  expect_snapshot(
    ard_categorical(data = ADSL,
                    by = "ARM",
                    variables = c("AGEGR1","SEX"),
                    stat_labels = everything() ~ list(c("n","p") ~ "n (pct)")) |>
      dplyr::filter(stat_name %in% c("n","p")) |>
      dplyr::select(stat_name, stat_label) |>
      unique()
  )

  # list
  expect_snapshot(
    ard_categorical(data = ADSL,
                    by = "ARM",
                    variables = c("AGEGR1","SEX"),
                    stat_labels = everything() ~ list(n = "num", p = "pct")) |>
      dplyr::filter(stat_name %in% c("n","p")) |>
      dplyr::select(stat_name, stat_label) |>
      unique()
  )

  # variable-specific
  expect_snapshot(
    ard_categorical(data = ADSL,
                    by = "ARM",
                    variables = c("AGEGR1","SEX"),
                    stat_labels = AGEGR1 ~ list(c("n","p") ~ "n (pct)")) |>
      dplyr::filter(stat_name %in% c("n","p")) |>
      dplyr::select(variable, stat_name, stat_label) |>
      unique()
  )
})


test_that("ard_categorical() messaging", {
  # denominator arg must have by column
  expect_snapshot(
    ard_categorical(
      mtcars,
      by = cyl,
      variables = am,
      denominator = iris
    ),
    error = TRUE
  )
})
