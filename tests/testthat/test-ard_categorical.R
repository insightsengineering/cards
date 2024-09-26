test_that("ard_categorical() univariate", {
  expect_error(
    ard_cat_uni <- ard_categorical(mtcars, variables = "am"),
    NA
  )
  expect_snapshot(class(ard_cat_uni))

  expect_equal(
    ard_cat_uni |>
      dplyr::filter(stat_name %in% "n") |>
      dplyr::pull(stat) |>
      as.integer(),
    table(mtcars$am) |> as.integer()
  )

  expect_equal(
    ard_cat_uni |>
      dplyr::filter(stat_name %in% "p") |>
      dplyr::pull(stat) |>
      as.numeric(),
    table(mtcars$am) |> prop.table() |> as.numeric()
  )

  expect_equal(
    dplyr::filter(ard_cat_uni, stat_name %in% "N")$stat[[1]],
    sum(!is.na(mtcars$am))
  )

  expect_equal(
    ard_categorical(
      mtcars,
      variables = starts_with("xxxxx")
    ),
    dplyr::tibble() |> as_card()
  )

  # works for ordered factors
  expect_equal(
    ard_categorical(
      mtcars |> dplyr::mutate(cyl = factor(cyl, ordered = TRUE)),
      variables = cyl
    ) |>
      dplyr::select(stat_name, stat_label, stat),
    ard_categorical(
      mtcars |> dplyr::mutate(cyl = factor(cyl, ordered = FALSE)),
      variables = cyl
    ) |>
      dplyr::select(stat_name, stat_label, stat)
  )

  expect_equal(
    ard_categorical(
      mtcars |> dplyr::mutate(cyl = factor(cyl, ordered = TRUE)),
      by = vs,
      variables = cyl
    ) |>
      dplyr::select(stat_name, stat_label, stat),
    ard_categorical(
      mtcars |> dplyr::mutate(cyl = factor(cyl, ordered = FALSE)),
      by = vs,
      variables = cyl
    ) |>
      dplyr::select(stat_name, stat_label, stat)
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
      dplyr::filter(stat_name %in% "n") |>
      dplyr::pull(stat) |>
      as.integer(),
    table(mtcars$am) |> as.integer()
  )

  expect_equal(
    ard_cat_new_denom |>
      dplyr::filter(stat_name %in% "p") |>
      dplyr::pull(stat) |>
      as.numeric(),
    table(mtcars$am) |> prop.table() |> as.numeric() %>% `/`(100) # styler: off
  )

  expect_equal(
    dplyr::filter(ard_cat_new_denom, stat_name %in% "N")$stat[[1]],
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
    apply_fmt_fn() |>
    dplyr::select(variable, variable_level, stat_name, stat, stat_fmt) |>
    as.data.frame() |>
    expect_snapshot()

  ard_categorical(
    mtcars,
    variables = c("am", "vs"),
    fmt_fn = list(
      am = list(p = function(x) round5(x * 100, digits = 3)),
      vs = list(p = function(x) round5(x * 100, digits = 1))
    )
  ) |>
    apply_fmt_fn() |>
    dplyr::select(variable, variable_level, stat_name, stat, stat_fmt) |>
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
      dplyr::filter(
        group2_level %in% "EYE DISORDERS",
        group3_level %in% "NASAL MUCOSA BIOPSY"
      ) |>
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
      dplyr::pull(stat) |>
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
      dplyr::pull(stat) |>
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
      dplyr::pull(stat) |>
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

  # addressing a sort edge case reported here: https://github.com/ddsjoberg/gtsummary/issues/1889
  expect_silent(
    ard_sort_test <-
      iris |>
      dplyr::mutate(
        trt = rep_len(c("Bladder + RP LN", "Bladder + Renal Fossa"), length.out = dplyr::n())
      ) |>
      ard_categorical(variables = trt, by = Species)
  )
  expect_s3_class(ard_sort_test$group1_level[[1]], "factor")
})

test_that("ard_categorical(stat_label) argument works", {
  # formula
  expect_snapshot(
    ard_categorical(
      data = ADSL,
      by = "ARM",
      variables = c("AGEGR1", "SEX"),
      stat_label = everything() ~ list(c("n", "p") ~ "n (pct)")
    ) |>
      as.data.frame() |>
      dplyr::filter(stat_name %in% c("n", "p")) |>
      dplyr::select(stat_name, stat_label) |>
      unique()
  )

  # list
  expect_snapshot(
    ard_categorical(
      data = ADSL,
      by = "ARM",
      variables = c("AGEGR1", "SEX"),
      stat_label = everything() ~ list(n = "num", p = "pct")
    ) |>
      as.data.frame() |>
      dplyr::filter(stat_name %in% c("n", "p")) |>
      dplyr::select(stat_name, stat_label) |>
      unique()
  )

  # variable-specific
  expect_snapshot(
    ard_categorical(
      data = ADSL,
      by = "ARM",
      variables = c("AGEGR1", "SEX"),
      stat_label = AGEGR1 ~ list(c("n", "p") ~ "n (pct)")
    ) |>
      as.data.frame() |>
      dplyr::filter(stat_name %in% c("n", "p")) |>
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
      dplyr::pull(stat) |>
      getElement(1),
    mtrx_conts["<65", "Placebo"]
  )

  expect_equal(
    ard_crosstab |>
      dplyr::filter(group1_level %in% "Placebo", variable_level %in% "<65", stat_name %in% "p") |>
      dplyr::pull(stat) |>
      getElement(1),
    mtrx_percs["<65", "Placebo"]
  )

  # works with an all missing variable
  df_missing <-
    dplyr::tibble(
      all_na_lgl = c(NA, NA),
      all_na_fct = factor(all_na_lgl, levels = letters[1:2]),
      letters = letters[1:2]
    )
  expect_equal(
    ard_categorical(
      data = df_missing,
      variables = c(all_na_lgl, all_na_fct),
      statistic = ~ c("n", "N"),
      denominator = "cell"
    ) |>
      dplyr::pull(stat) |>
      unlist(),
    rep_len(0L, length.out = 8L)
  )

  expect_equal(
    ard_categorical(
      data = df_missing,
      variables = c(all_na_lgl, all_na_fct),
      by = letters,
      statistic = ~ c("n", "N"),
      denominator = "cell"
    ) |>
      dplyr::pull(stat) |>
      unlist(),
    rep_len(0L, length.out = 16L)
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
      dplyr::pull(stat) |>
      unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    xtab_percent[rownames(xtab_percent) %in% "<65", colnames(xtab_percent) %in% "Placebo"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% "<65", group1_level %in% "Placebo", stat_name %in% "p") |>
      dplyr::pull(stat) |>
      unlist(),
    ignore_attr = TRUE
  )

  expect_equal(
    xtab_count[rownames(xtab_count) %in% ">80", colnames(xtab_count) %in% "Xanomeline Low Dose"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% ">80", group1_level %in% "Xanomeline Low Dose", stat_name %in% "n") |>
      dplyr::pull(stat) |>
      unlist(),
    ignore_attr = TRUE
  )
  expect_equal(
    xtab_percent[rownames(xtab_percent) %in% ">80", colnames(xtab_percent) %in% "Xanomeline Low Dose"],
    ard_crosstab_row |>
      dplyr::filter(variable_level %in% ">80", group1_level %in% "Xanomeline Low Dose", stat_name %in% "p") |>
      dplyr::pull(stat) |>
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
        statistic = list(AGEGR1 = c("n", "N")),
        fmt_fn = list(AGEGR1 = list("n" = 2))
      ),
    NA
  )

  expect_snapshot(
    ard_with_args |>
      apply_fmt_fn() |>
      dplyr::select(-fmt_fn, -warning, -error) |>
      as.data.frame()
  )

  # works with an all missing variable
  df_missing <- dplyr::tibble(all_na_lgl = c(NA, NA), letters = letters[1:2])
  expect_equal(
    ard_categorical(
      data = df_missing,
      variable = all_na_lgl,
      statistic = ~ c("n", "N"),
      denominator = "row"
    ) |>
      dplyr::pull(stat) |>
      unlist(),
    rep_len(0L, length.out = 4L)
  )

  expect_equal(
    ard_categorical(
      data = df_missing,
      variable = all_na_lgl,
      by = letters,
      statistic = ~ c("n", "N"),
      denominator = "row"
    ) |>
      dplyr::pull(stat) |>
      unlist(),
    rep_len(0L, length.out = 8L)
  )
})

test_that("ard_categorical(denominator='column') works", {
  expect_equal(
    ard_categorical(ADSL, variables = "AGEGR1", by = "ARM", denominator = "column") |>
      dplyr::select(all_ard_groups(), all_ard_variables(), stat_name, stat),
    ard_categorical(ADSL, variables = "AGEGR1", by = "ARM") |>
      dplyr::select(all_ard_groups(), all_ard_variables(), stat_name, stat)
  )

  # works with an all missing variable
  df_missing <- dplyr::tibble(all_na_lgl = c(NA, NA), letters = letters[1:2])
  expect_equal(
    ard_categorical(
      data = df_missing,
      variable = all_na_lgl,
      statistic = ~ c("n", "N"),
      denominator = "column"
    ) |>
      dplyr::pull(stat) |>
      unlist(),
    rep_len(0L, length.out = 4L)
  )

  expect_equal(
    ard_categorical(
      data = df_missing,
      variable = all_na_lgl,
      by = letters,
      statistic = ~ c("n", "N"),
      denominator = "column"
    ) |>
      dplyr::pull(stat) |>
      unlist(),
    rep_len(0L, length.out = 8L)
  )

  # works with an all missing variable
  df_missing <- dplyr::tibble(all_na_lgl = c(NA, NA), letters = letters[1:2])
  expect_equal(
    ard_categorical(
      data = df_missing,
      variable = all_na_lgl,
      statistic = ~ c("n", "N"),
      denominator = "column"
    ) |>
      dplyr::pull(stat) |>
      unlist(),
    rep_len(0L, length.out = 4L)
  )

  expect_equal(
    ard_categorical(
      data = df_missing,
      variable = all_na_lgl,
      by = letters,
      statistic = ~ c("n", "N"),
      denominator = "column"
    ) |>
      dplyr::pull(stat) |>
      unlist(),
    rep_len(0L, length.out = 8L)
  )
})

test_that("ard_categorical(denominator=integer()) works", {
  expect_equal(
    ard_categorical(ADSL, variables = AGEGR1, denominator = 1000) |>
      get_ard_statistics(variable_level %in% "<65", .attributes = NULL),
    list(n = 33, N = 1000, p = 33 / 1000)
  )
})

test_that("ard_categorical(denominator=<data frame with counts>) works", {
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
      dplyr::select(-fmt_fn),
    ard_categorical(
      ADSL,
      by = ARM,
      variables = AGEGR1
    ) |>
      dplyr::select(-fmt_fn)
  )
})

test_that("ard_categorical(denominator=<data frame without counts>) works", {
  expect_equal(
    ADSL |>
      dplyr::mutate(AGEGR1 = NA) |>
      ard_categorical(
        variables = AGEGR1,
        statistic = ~ c("n", "p"),
        denominator = rep_len(list(ADSL), 10L) |> dplyr::bind_rows()
      ) |>
      dplyr::pull(stat) |>
      unlist() |>
      unique(),
    0L
  )


  expect_equal(
    ADSL |>
      dplyr::mutate(AGEGR1 = NA) |>
      ard_categorical(
        variables = AGEGR1,
        by = ARM,
        statistic = ~ c("n", "p"),
        denominator = rep_len(list(ADSL), 10L) |> dplyr::bind_rows()
      ) |>
      dplyr::pull(stat) |>
      unlist() |>
      unique(),
    0L
  )
})

test_that("ard_categorical() and ARD column names", {
  ard_colnames <- c(
    "group1", "group1_level", "variable", "variable_level",
    "context", "stat_name", "stat_label", "stat",
    "fmt_fn", "warning", "error"
  )

  # no errors when these variables are the summary vars
  expect_error(
    {
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
      )
    },
    NA
  )

  # no errors when these vars are the by var
  expect_error(
    {
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
      )
    },
    NA
  )
})

test_that("ard_categorical() with grouped data works", {
  expect_equal(
    ADSL |>
      dplyr::group_by(ARM) |>
      ard_categorical(variables = AGEGR1),
    ard_categorical(data = ADSL, by = "ARM", variables = "AGEGR1")
  )
})


test_that("ard_categorical() and all NA columns", {
  expect_snapshot(
    error = TRUE,
    ADSL |>
      dplyr::mutate(AGEGR1 = NA_character_) |>
      ard_categorical(variables = AGEGR1)
  )
})

test_that("ard_categorical() can handle non-syntactic column names", {
  expect_equal(
    ADSL |>
      dplyr::mutate(`Age Group` = AGEGR1) |>
      ard_categorical(variables = `Age Group`) |>
      dplyr::select(stat),
    ADSL |>
      ard_categorical(variables = AGEGR1) |>
      dplyr::select(stat)
  )

  expect_equal(
    ADSL |>
      dplyr::mutate(`Age Group` = AGEGR1) |>
      ard_categorical(variables = "Age Group") |>
      dplyr::select(stat, error),
    ADSL |>
      ard_categorical(variables = AGEGR1) |>
      dplyr::select(stat, error)
  )

  expect_equal(
    ADSL |>
      dplyr::mutate(`Arm Var` = ARM, `Age Group` = AGEGR1) |>
      ard_categorical(by = `Arm Var`, variables = "Age Group") |>
      dplyr::select(stat, error),
    ADSL |>
      ard_categorical(by = ARM, variables = AGEGR1) |>
      dplyr::select(stat, error)
  )


  expect_equal(
    ADSL |>
      dplyr::mutate(`Arm Var` = ARM, `Age Group` = AGEGR1) |>
      ard_categorical(strata = "Arm Var", variables = `Age Group`) |>
      dplyr::select(stat, error),
    ADSL |>
      ard_categorical(strata = ARM, variables = AGEGR1) |>
      dplyr::select(stat, error)
  )
})

test_that("ard_categorical(strata) returns results in proper order", {
  expect_equal(
    ard_categorical(
      ADAE |>
        dplyr::arrange(AESEV != "SEVERE") |> # put SEVERE at the top
        dplyr::mutate(AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))) |>
        dplyr::mutate(ANY_AE = 1L),
      by = TRTA,
      strata = AESEV,
      variables = ANY_AE,
      denominator = ADSL |> dplyr::rename(TRTA = ARM)
    ) |>
      dplyr::select(group2_level) |>
      unlist() |>
      unique() |>
      as.character(),
    c("MILD", "MODERATE", "SEVERE")
  )
})

test_that("ard_categorical(by) messages about protected names", {
  mtcars2 <- mtcars |>
    dplyr::mutate(
      variable = am,
      variable_level = cyl,
      by = am,
      by_level = cyl
    )

  expect_snapshot(
    error = TRUE,
    ard_categorical(mtcars2, by = variable, variables = gear)
  )

  expect_error(
    ard_categorical(mtcars2, by = variable_level, variables = gear),
    'The `by` argument cannot include variables named "variable" and "variable_level".'
  )
})

# - test if function parameters can be used as variable names without error
test_that("ard_categorical() works when using generic names ", {
  # rename some variables
  mtcars2 <- mtcars %>%
    dplyr::rename("variable" = am, "variable_level" = cyl, "by" = disp, "group1_level" = gear)

  expect_equal(
    ard_categorical(mtcars, variables = c(am, cyl), by = disp, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(variable, variable_level), by = by, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(cyl, am), by = gear, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(variable_level, variable), by = group1_level, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(gear, am), by = disp, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(group1_level, variable), by = by, denominator = "row") |> dplyr::select(stat)
  )

  # rename vars
  mtcars2 <- mtcars %>%
    dplyr::rename("N" = am, "p" = cyl, "name" = disp, "group1_level" = gear)

  expect_equal(
    ard_categorical(mtcars, variables = c(am, cyl), by = disp, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(N, p), by = name, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(disp, gear), by = am, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(name, group1_level), by = N, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(am, disp), by = gear, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(N, name), by = group1_level, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(am, disp), by = cyl, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(N, name), by = p, denominator = "row") |> dplyr::select(stat)
  )

  # rename vars
  mtcars2 <- mtcars %>%
    dplyr::rename("n" = am, "mean" = cyl, "p.std.error" = disp, "n_unweighted" = gear)

  expect_equal(
    ard_categorical(mtcars, variables = c(gear, cyl), by = disp, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(n_unweighted, mean), by = p.std.error, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(gear, cyl), by = am, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(n_unweighted, mean), by = n, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(am, disp), by = cyl, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(n, p.std.error), by = mean, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(am, disp), by = gear, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(n, p.std.error), by = n_unweighted, denominator = "row") |> dplyr::select(stat)
  )

  # rename vars
  mtcars2 <- mtcars %>%
    dplyr::rename("N_unweighted" = am, "p_unweighted" = cyl, "column" = disp, "row" = gear)

  expect_equal(
    ard_categorical(mtcars, variables = c(am, cyl), by = disp, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(N_unweighted, p_unweighted), by = column, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(disp, gear), by = am, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(column, row), by = N_unweighted, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(am, disp), by = cyl, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(N_unweighted, column), by = p_unweighted, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(mtcars, variables = c(am, disp), by = gear, denominator = "row") |> dplyr::select(stat),
    ard_categorical(mtcars2, variables = c(N_unweighted, column), by = row, denominator = "row") |> dplyr::select(stat)
  )
})

test_that("ard_categorical(by) messages about protected names", {
  mtcars2 <- mtcars %>%
    dplyr::rename("variable" = am, "variable_level" = cyl, "by" = disp, "group1_level" = gear)

  expect_snapshot(
    error = TRUE,
    ard_categorical(mtcars2, by = variable, variables = by)
  )

  expect_error(
    ard_categorical(mtcars2, by = variable_level, variables = by),
    'The `by` argument cannot include variables named "variable" and "variable_level".'
  )
})


test_that("ard_categorical() follows ard structure", {
  expect_silent(
    ard_categorical(mtcars, variables = "am") |>
      check_ard_structure(method = FALSE)
  )
})

test_that("ard_categorical() with hms times", {
  # originally reported in https://github.com/ddsjoberg/gtsummary/issues/1893
  skip_if_not_installed("hms")
  withr::local_package("hms")

  ADSL2 <-
    ADSL |>
    dplyr::mutate(time_hms = hms(seconds = 15))
  expect_silent(
    ard <- ard_categorical(ADSL2, by = ARM, variables = time_hms)
  )
  expect_equal(
    ard$stat,
    ard_categorical(
      ADSL2 |> dplyr::mutate(time_hms = as.numeric(time_hms)),
      by = ARM,
      variables = time_hms
    )$stat
  )
})

test_that("ard_categorical() errors with incomplete factor columns", {
  # Check error when factors have no levels
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = character(0))) |>
      ard_categorical(variables = am)
  )

  # Check error when factor has NA level
  expect_snapshot(
    error = TRUE,
    mtcars |>
      dplyr::mutate(am = factor(am, levels = c(0, 1, NA), exclude = NULL)) |>
      ard_categorical(variables = am)
  )
})
