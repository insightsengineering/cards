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

  # check for messaging about missing by/strata combos in denominator arg
  expect_snapshot(
    error = TRUE,
    ard_categorical(
      ADSL,
      by = "ARM",
      variables = "AGEGR1",
      denominator = ADSL |> dplyr::filter(ARM %in% "Placebo")
    )
  )

})

test_that("ard_categorical(stat_labels) argument works", {
  # formula
  expect_snapshot(
    ard_categorical(data = ADSL,
                    by = "ARM",
                    variables = c("AGEGR1","SEX"),
                    stat_labels = everything() ~ list(c("n","p") ~ "n (pct)")) |>
      as.data.frame() |>
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
      as.data.frame() |>
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
      as.data.frame() |>
      dplyr::filter(stat_name %in% c("n","p")) |>
      dplyr::select(variable, stat_name, stat_label) |>
      unique()
  )
})


test_that("ard_categorical(denominator='cell') works", {
  expect_error(
    ard_crosstab <- ard_categorical(ADSL, variables = "AGEGR1", by = "ARM", denominator = "cell"),
    NA
  )

  mtrx_conts <- with(ADSL, table(AGEGR1, ARM)) |> unclass()
  mtrx_percs <- mtrx_conts / sum(mtrx_conts)

  expect_equal(
    ard_crosstab |>
      dplyr::filter(group1_level %in% "Placebo", variable_level %in% "<65", stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      getElement(1),
    mtrx_conts["<65", "Placebo"]
  )

  expect_equal(
    ard_crosstab |>
      dplyr::filter(group1_level %in% "Placebo", variable_level %in% "<65", stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      getElement(1),
    mtrx_percs["<65", "Placebo"]
  )
})

test_that("ard_categorical(denominator='row') works", {
  expect_error(
    ard_crosstab_row <- ard_categorical(ADSL, variables = "AGEGR1", by = "ARM", denominator = "row"),
    NA
  )

  xtab_count <- with(ADSL, table(AGEGR1, ARM))
  xtab_percent <- proportions(xtab_count, margin = 1)

  expect_equal(
    xtab_count[rownames(xtab_count) %in% "<65", colnames(xtab_count) %in% "Placebo"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% "<65", group1_level %in% "Placebo", stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    xtab_percent[rownames(xtab_percent) %in% "<65", colnames(xtab_percent) %in% "Placebo"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% "<65", group1_level %in% "Placebo", stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    xtab_count[rownames(xtab_count) %in% ">80", colnames(xtab_count) %in% "Xanomeline Low Dose"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% ">80", group1_level %in% "Xanomeline Low Dose", stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    xtab_percent[rownames(xtab_percent) %in% ">80", colnames(xtab_percent) %in% "Xanomeline Low Dose"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% ">80", group1_level %in% "Xanomeline Low Dose", stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      unlist(),
    ignore_attr = TRUE
  )

  # testing the arguments work properly
  expect_error(
    ard_with_args <-
      ard_categorical(
        ADSL,
        variables = "AGEGR1", by = "ARM",
        denominator = "row",
        statistics = list(AGEGR1 = categorical_variable_summary_fns(c("n", "N"))),
        fmt_fn = list(AGEGR1 = list("n" = 2))
      ),
    NA
  )

  expect_snapshot(
    ard_with_args |>
      apply_statistic_fmt_fn() |>
      flatten_ard() |>
      dplyr::select(-statistic_fmt_fn, -warning, -error) |>
      as.data.frame()
  )
})

test_that("ard_categorical(denominator='column') works", {
  expect_equal(
    ard_categorical(ADSL, variables = "AGEGR1", by = "ARM", denominator = "column") |>
      dplyr::select(all_ard_groups(), all_ard_variables(), stat_name, statistic),
    ard_categorical(ADSL, variables = "AGEGR1", by = "ARM") |>
      dplyr::select(all_ard_groups(), all_ard_variables(), stat_name, statistic)
  )
})

test_that("ard_categorical(denominator=integer()) works", {
  expect_equal(
    ard_categorical(ADSL, variables = AGEGR1, denominator = 1000) |>
      get_ard_statistics(variable_level %in% "<65", .attributes = NULL),
    list(n = 33, N = 1000, p = 33/1000)
  )
})

test_that("ard_categorical(denominator=<data frame with counts>) works", {
  expect_equal(
    ard_categorical(
      ADSL,
      by = ARM,
      variables = AGEGR1,
      denominator =
        data.frame(
          ARM = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"),
          ...ard_N... = c(86, 84, 84)
        )
    ) |>
      flatten_ard(),
    ard_categorical(
      ADSL,
      by = ARM,
      variables = AGEGR1
    ) |>
      flatten_ard()
  )

  expect_snapshot(
    error = TRUE,
    ard_categorical(
      ADSL,
      by = ARM,
      variables = AGEGR1,
      denominator =
        data.frame(
          ARM = c("Placebo", "Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"),
          ...ard_N... = c(86, 86, 84, 84)
        )
    )
  )

  expect_snapshot(
    error = TRUE,
    ard_categorical(
      ADSL,
      by = ARM,
      variables = AGEGR1,
      denominator = data.frame(ARM = "Placebo", ...ard_N... = 86)
    )
  )
})

test_that("ard_categorical(statistics) works with custom fns", {
  expect_snapshot(
    ard_custom_fns <-
      ard_categorical(
        ADSL,
        variables = AGEGR1,
        statistics =
          ~categorical_variable_summary_fns(
            other_stats = list(mode = function(x) table(x) |> sort(decreasing = TRUE) |> names() |> getElement(1),
                               length = function(x) length(x))
          )
      ) |>
      flatten_ard()
  )

  expect_equal(
    ard_custom_fns |>
      dplyr::select(-variable_level) |>
      dplyr::filter(stat_name %in% c("mode", "length")),
    ard_categorical(
      ADSL,
      variables = AGEGR1,
      statistics =
        ~categorical_variable_summary_fns(
          summaries = list(),
          other_stats = list(mode = function(x) table(x) |> sort(decreasing = TRUE) |> names() |> getElement(1),
                             length = function(x) length(x))
        )
    ) |>
      flatten_ard()
  )
})

test_that("ard_categorical() and ARD column names", {
  ard_colnames <- c("group1", "group1_level", "variable", "variable_level",
                    "context", "stat_name", "stat_label", "statistic",
                    "statistic_fmt_fn", "warning", "error")

  # no errors when these variables are the summary vars
  expect_error({
    lapply(
      ard_colnames,
      function(var) {
        df <- mtcars[c("am", "cyl")]
        names(df) <- c("am", var)
        ard_categorical(
          data = df,
          by = "am",
          variables = all_of(var)
        )
      }
    )},
    NA
  )

  # no errors when these vars are the by var
  expect_error({
    lapply(
      ard_colnames,
      function(byvar) {
        df <- mtcars[c("am", "cyl")]
        names(df) <- c(byvar, "cyl")
        ard_continuous(
          data = df,
          by = all_of(byvar),
          variables = "cyl"
        )
      }
    )},
    NA
  )
})
