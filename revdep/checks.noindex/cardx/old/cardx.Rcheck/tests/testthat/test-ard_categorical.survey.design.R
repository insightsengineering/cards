skip_if_not(is_pkg_installed("survey"))

# Test survey.design working (2x3)
test_that("ard_categorical.survey.design() works", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

  # setup for value checks
  df_titanic <- as.data.frame(Titanic) |> tidyr::uncount(weights = Freq)

  # denom = row, with by
  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  # denom = column, with by
  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_col, method = FALSE))

  # denom = cell, with by
  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # check the calculated stats are correct

  # section 1: by variable, row denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row |> dplyr::arrange_all(), stat_name %in% "n") |> unlist(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row |> dplyr::arrange_all(), stat_name %in% "N") |> unlist(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row |> dplyr::arrange_all(), stat_name %in% "p") |> unlist(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row |> dplyr::arrange_all(), stat_name %in% "p.std.error") |> unlist() |> unname() |> sort(),
    unname(c(
      survey::svyby(
        formula = reformulate2("Survived"),
        by = reformulate2("Age"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.SurvivedYes", "se.SurvivedNo")] |> unlist(),
      survey::svyby(
        formula = reformulate2("Survived"),
        by = reformulate2("Class"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.SurvivedYes", "se.SurvivedNo")] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row |> dplyr::arrange_all(), stat_name %in% "n_unweighted") |> unlist() |> unname(),
    ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = Survived, denominator = "row") |> dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N_unweighted") |> unlist() |> unname(),
    ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p_unweighted") |> unlist() |> unname(),
    ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname()
  )

  # section 2: by variable, column denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col |> dplyr::arrange_all(), stat_name %in% "n") |> unlist(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col |> dplyr::arrange_all(), stat_name %in% "N") |> unlist(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col |> dplyr::arrange_all(), stat_name %in% "p") |> unlist(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    unname(cards::get_ard_statistics(ard_svy_cat_col |> dplyr::arrange_all(), stat_name %in% "p.std.error")) |> unlist() |> sort(),
    unname(c(
      survey::svyby(
        formula = reformulate2("Age"),
        by = reformulate2("Survived"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.AgeAdult", "se.AgeChild")] |> unlist(),
      survey::svyby(
        formula = reformulate2("Class"),
        by = reformulate2("Survived"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.Class1st", "se.Class2nd", "se.Class3rd", "se.ClassCrew")] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n_unweighted") |> unlist() |> unname(),
    ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N_unweighted") |> unlist() |> unname(),
    ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p_unweighted") |> unlist() |> unname(),
    ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname()
  )

  # section 3: by variable, cell denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell |> dplyr::arrange_all(), stat_name %in% "n") |> unlist(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell |> dplyr::arrange_all(), stat_name %in% "N") |> unlist(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell |> dplyr::arrange_all(), stat_name %in% "p") |> unlist(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      dplyr::arrange_all() |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell |> dplyr::arrange_all(), stat_name %in% "p.std.error") |> unlist() |> unname() |> sort(),
    unname(c(
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("Survived")), !!sym(bt("Class")))),
        design = svy_titanic,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[, "SE"] |> unlist(),
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("Survived")), !!sym(bt("Age")))),
        design = svy_titanic,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[, "SE"] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n_unweighted") |> unlist() |> unname(),
    ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N_unweighted") |> unlist() |> unname(),
    ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p_unweighted") |> unlist() |> unname(),
    ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname()
  )


  # denom = row, without by
  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  # denom = column, without by
  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # denom = cell, without by
  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # check the calculated stats are correct

  # section 4: without by variable, row denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row |> dplyr::arrange_all(), stat_name %in% "p.std.error") |> unlist() |> unname() |> sort(),
    c(dplyr::tibble(
      variable_level = unique(svy_titanic$variables[["Class"]]) |> sort() |> as.character(),
      p = 1,
      p.std.error = 0,
      deff = NaN
    ) |> dplyr::select("p.std.error") |> unlist() |> unname(), dplyr::tibble(
      variable_level = unique(svy_titanic$variables[["Age"]]) |> sort() |> as.character(),
      p = 1,
      p.std.error = 0,
      deff = NaN
    ) |> dplyr::select("p.std.error") |> unlist() |> unname())
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n_unweighted") |> unlist() |> unname(),
    cards::ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N_unweighted") |> unlist() |> unname(),
    cards::ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p_unweighted") |> unlist() |> unname(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname()
  )

  # section 5: without by variable, column denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p.std.error") |> unlist() |> unname(),
    c(
      survey::svymean(reformulate2("Class"), design = svy_titanic, na.rm = TRUE, deff = "Design Effect") |>
        dplyr::as_tibble(rownames = "var_level") |>
        dplyr::select("SE") |> unlist() |> unname(),
      survey::svymean(reformulate2("Age"), design = svy_titanic, na.rm = TRUE, deff = "Design Effect") |>
        dplyr::as_tibble(rownames = "var_level") |>
        dplyr::select("SE") |> unlist() |> unname()
    )
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n_unweighted") |> unlist() |> unname(),
    cards::ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N_unweighted") |> unlist() |> unname(),
    cards::ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p_unweighted") |> unlist() |> unname(),
    cards::ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = NULL, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname()
  )

  # section 6: without by variable, cell denominator
  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p.std.error") |> unlist() |> unname(),
    c(
      survey::svymean(reformulate2("Class"), design = svy_titanic, na.rm = TRUE, deff = "Design Effect") |>
        dplyr::as_tibble(rownames = "var_level") |>
        dplyr::select("SE") |> unlist() |> unname(),
      survey::svymean(reformulate2("Age"), design = svy_titanic, na.rm = TRUE, deff = "Design Effect") |>
        dplyr::as_tibble(rownames = "var_level") |>
        dplyr::select("SE") |> unlist() |> unname()
    )
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n_unweighted") |> unlist() |> unname(),
    cards::ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N_unweighted") |> unlist() |> unname(),
    cards::ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p_unweighted") |> unlist() |> unname(),
    cards::ard_categorical(as.data.frame(Titanic), variables = c(Class, Age), by = NULL, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname()
  )
})

test_that("ard_categorical.survey.design() returns an error when variables have all NAs", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

  # row denom
  svy_titanic$variables$Class <- NA

  expect_snapshot(
    ard_categorical(
      svy_titanic,
      variables = c(Class, Age),
      by = Survived,
      denominator = "row"
    ),
    error = TRUE
  )

  # column denom
  expect_snapshot(
    ard_categorical(
      svy_titanic,
      variables = c(Class, Age),
      by = Survived,
      denominator = "column"
    ),
    error = TRUE
  )

  # cell denom
  expect_snapshot(
    ard_categorical(
      svy_titanic,
      variables = c(Class, Age),
      by = Survived,
      denominator = "cell"
    ),
    error = TRUE
  )
})

# - Do we get results for unobserved factor levels in the `by` and `variable` variables?
test_that("ard_categorical.survey.design() works for unobserved factor levels", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Survived <- fct_expand(svy_titanic$variables$Survived, "Unknown")

  # data setup for equality checks
  df_titanic <- as.data.frame(Titanic) |> tidyr::uncount(weights = Freq)
  df_titanic$Survived <- fct_expand(df_titanic$Survived, "Unknown")

  # for unweighted <-
  df_uw <- as.data.frame(Titanic)
  df_uw$Survived <- fct_expand(df_uw$Survived, "Unknown")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_col, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n") |> unlist() |> sort(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row |> dplyr::arrange_all(), stat_name %in% "p.std.error") |> unlist() |> unname() |> sort(),
    unname(c(
      survey::svyby(
        formula = reformulate2("Survived"),
        by = reformulate2("Age"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.SurvivedYes", "se.SurvivedNo", "se.SurvivedUnknown")] |> unlist(),
      survey::svyby(
        formula = reformulate2("Survived"),
        by = reformulate2("Class"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.SurvivedYes", "se.SurvivedNo", "se.SurvivedUnknown")] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> sort()
  )

  expect_equal(
    unname(cards::get_ard_statistics(ard_svy_cat_col |> dplyr::arrange_all(), stat_name %in% "p.std.error")) |> unlist() |> sort(),
    unname(c(
      survey::svyby(
        formula = reformulate2("Age"),
        by = reformulate2("Survived"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.AgeAdult", "se.AgeChild")] |> unlist(),
      survey::svyby(
        formula = reformulate2("Class"),
        by = reformulate2("Survived"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.Class1st", "se.Class2nd", "se.Class3rd", "se.ClassCrew")] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell |> dplyr::arrange_all(), stat_name %in% "p.std.error") |> unlist() |> unname() |> sort(),
    unname(c(
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("Survived")), !!sym(bt("Class")))),
        design = svy_titanic,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[, "SE"] |> unlist(),
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("Survived")), !!sym(bt("Age")))),
        design = svy_titanic,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[, "SE"] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname() |> sort()
  )
  # variables have unobserved levels, no by variable
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Class <- fct_expand(svy_titanic$variables$Survived, "Peasant")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variable AND by have unobserved levels
  svy_titanic$variables$Survived <- fct_expand(svy_titanic$variables$Survived, "Unknown")

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))
})

# - Do we get results for unobserved logical levels in the `by` and `variable` variables?
test_that("ard_categorical.survey.design() works for unobserved logical levels", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Survived <- rep(TRUE, length(svy_titanic$variables$Survived))

  df_titanic <- as.data.frame(Titanic) |> tidyr::uncount(weights = Freq)
  df_titanic$Survived <- rep(TRUE, length(df_titanic$Survived))

  # for unweighted
  df_uw <- as.data.frame(Titanic)
  df_uw$Survived <- rep(TRUE, length(df_uw$Survived))

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_col, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n") |> unlist() |> sort(),
    ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row |> dplyr::arrange_all(), stat_name %in% "p.std.error") |> unlist() |> unname() |> sort(),
    unname(c(
      survey::svyby(
        formula = reformulate2("Survived"),
        by = reformulate2("Age"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.SurvivedFALSE", "se.SurvivedTRUE")] |> unlist(),
      survey::svyby(
        formula = reformulate2("Survived"),
        by = reformulate2("Class"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.SurvivedFALSE", "se.SurvivedTRUE")] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "n_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "N_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_row, stat_name %in% "p_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> sort()
  )

  expect_equal(
    unname(cards::get_ard_statistics(ard_svy_cat_col |> dplyr::arrange_all(), stat_name %in% "p.std.error")) |> unlist() |> sort(),
    unname(c(
      survey::svyby(
        formula = reformulate2("Age"),
        by = reformulate2("Survived"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.AgeAdult", "se.AgeChild")] |> unlist(),
      survey::svyby(
        formula = reformulate2("Class"),
        by = reformulate2("Survived"),
        design = svy_titanic,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c("se.Class1st", "se.Class2nd", "se.Class3rd", "se.ClassCrew")] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "n_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "N_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_col, stat_name %in% "p_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N") |> unlist(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p") |> unlist() |> sort(),
    cards::ard_categorical(df_titanic, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell |> dplyr::arrange_all(), stat_name %in% "p.std.error" & group1_level == TRUE) |> unlist() |> unname() |> sort(),
    unname(c(
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("Survived")), !!sym(bt("Class")))),
        design = svy_titanic,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[, "SE"] |> unlist(),
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("Survived")), !!sym(bt("Age")))),
        design = svy_titanic,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[, "SE"] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "n_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "N_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname() |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_svy_cat_cell, stat_name %in% "p_unweighted") |> unlist() |> unname() |> sort(),
    cards::ard_categorical(df_uw, variables = c(Class, Age), by = Survived, denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist() |> unname() |> sort()
  )

  # variables have unobserved levels, no by variable
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Age <- rep(TRUE, length(svy_titanic$variables$Age))

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variable AND by have unobserved levels
  svy_titanic$variables$Survived <- rep(TRUE, length(svy_titanic$variables$Survived))

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))
})

# - Does the work around apply for variables with only 1 level
test_that("ard_categorical.survey.design() works with variables with only 1 level", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Survived <- rep("Yes", length(svy_titanic$variables$Survived))

  # by variable only has 1 level
  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_col, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variables have only 1 level, no by variable
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)
  svy_titanic$variables$Age <- as.factor(rep("Child", length(svy_titanic$variables$Age)))

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  # variable AND by have only 1 level
  svy_titanic$variables$Survived <- as.factor(rep("Yes", length(svy_titanic$variables$Survived)))

  expect_error(
    ard_svy_cat_row <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_row, method = FALSE))

  expect_error(
    ard_svy_cat_col <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "column"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))

  expect_error(
    ard_svy_cat_cell <-
      ard_categorical(
        svy_titanic,
        variables = c(Class, Age),
        by = Survived,
        denominator = "cell"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_svy_cat_cell, method = FALSE))
})

test_that("ard_categorical.survey.design(by) messages about protected names", {
  svy_mtcars <-
    survey::svydesign(
      ids = ~1,
      data = mtcars |>
        dplyr::mutate(
          variable = am,
          variable_level = cyl,
          by = am,
          by_level = cyl,
          group1_level = disp,
          n = carb,
          p.std.error = drat,
          name = hp,
          p = vs
        ),
      weights = ~1
    )

  expect_snapshot(
    error = TRUE,
    ard_categorical(svy_mtcars, by = variable, variables = gear)
  )

  expect_error(
    ard_categorical(svy_mtcars, by = variable_level, variables = gear),
    'The `by` argument cannot include variables named "variable", "variable_level", "group1_level", "p", and "n".'
  )

  expect_snapshot(
    error = TRUE,
    ard_categorical(svy_mtcars, by = p.std.error, variables = name)
  )

  expect_error(
    ard_categorical(svy_mtcars, by = p.std.error, variables = name),
    'The `variables` argument cannot include variables named "by", "name", "n", "p", and "p.std.error".'
  )
})

# - test if function parameters can be used as variable names without error
test_that("ard_categorical.survey.design() works when using generic names ", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

  svy_titanic2 <- svy_titanic
  svy_titanic2$variables <- svy_titanic$variables %>%
    dplyr::rename("variable" = Class, "variable_level" = Age, "by" = Survived, "group1_level" = Sex)


  expect_equal(
    ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = c(variable, variable_level), by = by, denominator = "row") |> dplyr::select(stat)
  )

  # rename vars

  svy_titanic2$variables <- svy_titanic$variables %>%
    dplyr::rename("N" = Class, "p" = Age, "name" = Survived, "group1_level" = Sex)

  expect_equal(
    ard_categorical(svy_titanic, variables = c(Class), by = Survived, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = c(N), by = name, denominator = "row") |> dplyr::select(stat)
  )

  # rename vars
  svy_titanic2$variables <- svy_titanic$variables %>%
    dplyr::rename("n" = Class, "mean" = Age, "p.std.error" = Survived, "n_unweighted" = Sex)

  expect_equal(
    ard_categorical(svy_titanic, variables = c(Sex, Age), by = Survived, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = c(n_unweighted, mean), by = p.std.error, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(svy_titanic, variables = Sex, by = Survived, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = n_unweighted, by = p.std.error, denominator = "row") |> dplyr::select(stat)
  )

  # rename vars
  svy_titanic2$variables <- svy_titanic$variables %>%
    dplyr::rename("N_unweighted" = Class, "p_unweighted" = Age, "column" = Survived, "row" = Sex)

  expect_equal(
    ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = c(N_unweighted, p_unweighted), by = column, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(svy_titanic, variables = c(Survived, Sex), by = Class, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = c(column, row), by = N_unweighted, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(svy_titanic, variables = c(Class, Survived), by = Age, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = c(N_unweighted, column), by = p_unweighted, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(svy_titanic, variables = c(Class, Survived), by = Sex, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = c(N_unweighted, column), by = row, denominator = "row") |> dplyr::select(stat)
  )

  # rename vars
  svy_titanic2$variables <- svy_titanic$variables %>%
    dplyr::rename("cell" = Class, "p_unweighted" = Age, "column" = Survived, "row" = Sex)

  expect_equal(
    ard_categorical(svy_titanic, variables = c(Class, Survived), by = Age, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = c(cell, column), by = p_unweighted, denominator = "row") |> dplyr::select(stat)
  )

  expect_equal(
    ard_categorical(svy_titanic, variables = c(Sex, Survived), by = Class, denominator = "row") |> dplyr::select(stat),
    ard_categorical(svy_titanic2, variables = c(row, column), by = cell, denominator = "row") |> dplyr::select(stat)
  )
})

test_that("ard_categorical.survey.design(statistic) properly excluded unweighted stats not selected", {
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

  expect_equal(
    ard_categorical(
      svy_titanic,
      variables = Sex,
      statistic = ~ c("N", "N_unweighted")
    ) |>
      dplyr::select(variable, variable_level, stat_name, stat_label, stat),
    ard_categorical(
      svy_titanic,
      variables = Sex
    ) |>
      dplyr::filter(stat_name %in% c("N", "N_unweighted")) |>
      dplyr::select(variable, variable_level, stat_name, stat_label, stat)
  )
})

test_that("ard_categorical follows ard structure", {
  data(api, package = "survey")
  svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

  expect_silent(
    ard_categorical(
      svy_titanic,
      variables = c(Class, Age),
      by = Survived,
      denominator = "row"
    ) |>
      cards::check_ard_structure(method = FALSE)
  )
})

test_that("ard_categorical.survey.design() original types are retained", {
  svy_titanic <-
    survey::svydesign(
      ~1,
      data = as.data.frame(Titanic) |> dplyr::mutate(
        Class.dbl = as.numeric(Class),
        Class.int = as.integer(Class)
      ),
      weights = ~Freq
    )

  # factors and integer check
  expect_silent(
    ard <-
      ard_categorical(svy_titanic, variables = c(Class, Age, Class.int, Class.dbl), by = Survived)
  )
  expect_equal(
    unlist(ard$group1_level) |> levels(),
    levels(as.data.frame(Titanic)$Survived)
  )
  expect_true(
    dplyr::filter(ard, variable %in% "Class") |>
      dplyr::pull("variable_level") |>
      getElement(1L) |>
      is.factor()
  )
  expect_true(
    dplyr::filter(ard, variable %in% "Age") |>
      dplyr::pull("variable_level") |>
      getElement(1L) |>
      is.factor()
  )
  expect_true(
    dplyr::filter(ard, variable %in% "Class.int") |>
      dplyr::pull("variable_level") |>
      getElement(1L) |>
      is.integer()
  )
  expect_true(
    dplyr::filter(ard, variable %in% "Class.dbl") |>
      dplyr::pull("variable_level") |>
      getElement(1L) |>
      is.numeric()
  )
})

test_that("ard_categorical.survey.design() works with all NA fct variables", {
  expect_silent(
    ard_fct_na <-
      survey::svydesign(
        ~1,
        data =
          dplyr::tibble(
            fct = factor(c(NA, NA), levels = c("no", "yes")),
            lgl = c(NA, NA)
          ),
        weights = ~1
      ) |>
      ard_categorical(variables = fct)
  )

  # all Ns should be zero
  expect_equal(
    ard_fct_na |>
      dplyr::filter(!startsWith(stat_name, "p") & stat_name != "deff") |>
      dplyr::pull(stat) |>
      unlist() |>
      unique(),
    0L
  )

  # all percentages (and deff) should be NaN
  expect_true(
    ard_fct_na |>
      dplyr::filter((startsWith(stat_name, "p") | stat_name == "deff") & stat_name != "p.std.error") |>
      dplyr::pull(stat) |>
      unique() |>
      unlist() |>
      is.nan()
  )
})

test_that("ard_categorical.survey.design() messaging with all NA lgl variables", {
  expect_snapshot(
    error = TRUE,
    survey::svydesign(
      ~1,
      data =
        dplyr::tibble(
          fct = factor(c(NA, NA), levels = c("no", "yes")),
          lgl = c(NA, NA)
        ),
      weights = ~1
    ) |>
      ard_categorical(variables = lgl)
  )
})
