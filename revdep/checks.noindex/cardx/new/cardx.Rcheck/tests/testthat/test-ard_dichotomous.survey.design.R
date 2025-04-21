skip_if_not(is_pkg_installed("survey"))

# Test survey.design works
test_that("ard_dichotomous.survey.design() works", {
  svy_dicho <- survey::svydesign(ids = ~1, data = mtcars, weights = ~1)
  # convert variables to factor
  svy_dicho$variables <- svy_dicho$variables |>
    dplyr::mutate(across(c("cyl", "am", "vs"), as.factor))

  # row denom with by var
  expect_error(
    ard_dichotomous_row <-
      ard_dichotomous(svy_dicho,
        by = vs,
        variables = c(cyl, am),
        value = list(cyl = 4),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_row, method = FALSE))

  # col denom with by var
  expect_error(
    ard_dichotomous_col <- ard_dichotomous(svy_dicho,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "column"
    ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_col, method = FALSE))

  # cell denom with by var
  expect_error(
    ard_dichotomous_cell <- ard_dichotomous(svy_dicho,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "cell"
    ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_cell, method = FALSE))


  # test individual stats

  # section 1: by variable, row denominator
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "n") |> unlist(),
    ard_dichotomous(mtcars,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "row"
    ) |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "N") |> unlist(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "p") |> unlist(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row |> dplyr::arrange_all(), stat_name %in% "p.std.error") |> unlist() |> unname() |> sort(),
    c(
      survey::svyby(
        formula = reformulate2("vs"),
        by = reformulate2("am"),
        design = svy_dicho,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[2, c(4, 5)] |> unlist() |> unname(),
      survey::svyby(
        formula = reformulate2("vs"),
        by = reformulate2("cyl"),
        design = svy_dicho,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[1, c(4, 5)] |> unlist() |> unname()
    ) |>
      sort()
  )



  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "deff") |> unlist() |> unname() |> sort(),
    c(
      survey::svyby(
        formula = reformulate2("vs"),
        by = reformulate2("am"),
        design = svy_dicho,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[2, c(6, 7)] |> unlist() |> unname(),
      survey::svyby(
        formula = reformulate2("vs"),
        by = reformulate2("cyl"),
        design = svy_dicho,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[1, c(6, 7)] |> unlist() |> unname()
    ) |>
      sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "n_unweighted") |> unlist() |> unname(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_row, stat_name %in% "N_unweighted") |> unlist() |> unname(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "row") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname()
  )

  # section 2: by variable, column denominator
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "n") |> unlist(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "N") |> unlist(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "p") |> unlist(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col |> dplyr::arrange_all(), stat_name %in% "p.std.error") |> unlist() |> unname() |> sort(),
    c(
      survey::svyby(
        formula = reformulate2("am"),
        by = reformulate2("vs"),
        design = svy_dicho,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c(4)] |> unlist() |> unname(),
      survey::svyby(
        formula = reformulate2("cyl"),
        by = reformulate2("vs"),
        design = svy_dicho,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[, c(5)] |> unlist() |> unname()
    ) |>
      sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col |> dplyr::arrange_all(), stat_name %in% "deff") |> unlist() |> unname() |> sort(),
    c(
      survey::svyby(
        formula = reformulate2("am"),
        by = reformulate2("vs"),
        design = svy_dicho,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[1, c(6, 7)] |> unlist() |> unname(),
      survey::svyby(
        formula = reformulate2("cyl"),
        by = reformulate2("vs"),
        design = svy_dicho,
        FUN = survey::svymean,
        na.rm = TRUE,
        deff = "Design Effect"
      )[2, c(8, 9)] |> unlist() |> unname()
    ) |>
      sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "n_unweighted") |> unlist() |> unname(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname()
  )
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_col, stat_name %in% "N_unweighted") |> unlist() |> unname(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "column") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname()
  )

  # section 3: by variable, cell denominator
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "n") |> unlist(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "N") |> unlist(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "p") |> unlist(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "p") |> unlist()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell |> dplyr::arrange_all(), stat_name %in% "p.std.error") |> unlist() |> unname() |> sort(),
    unname(c(
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("vs")), !!sym(bt("cyl")))),
        design = svy_dicho,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[1:2, "SE"] |> unlist(),
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("vs")), !!sym(bt("am")))),
        design = svy_dicho,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[2:3, "SE"] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell |> dplyr::arrange_all(), stat_name %in% "deff") |> unlist() |> unname() |> sort(),
    unname(c(
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("vs")), !!sym(bt("cyl")))),
        design = svy_dicho,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[1:2, "deff"] |> unlist(),
      as.data.frame(survey::svymean(
        x = inject(~ interaction(!!sym(bt("vs")), !!sym(bt("am")))),
        design = svy_dicho,
        na.rm = TRUE,
        deff = "Design Effect"
      ))[2:3, "deff"] |> unlist()
    )) |> sort()
  )

  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "n_unweighted") |> unlist() |> unname(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "n") |> unlist() |> unname()
  )
  expect_equal(
    cards::get_ard_statistics(ard_dichotomous_cell, stat_name %in% "N_unweighted") |> unlist() |> unname(),
    ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4), denominator = "cell") |>
      cards::get_ard_statistics(stat_name %in% "N") |> unlist() |> unname()
  )

  # row denom without by var
  expect_error(
    ard_dichotomous_row <- ard_dichotomous(svy_dicho,
      by = NULL,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "row"
    ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_row, method = FALSE))

  # col denom without by var
  expect_error(
    ard_dichotomous_col <- ard_dichotomous(svy_dicho,
      by = NULL,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "column"
    ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_col, method = FALSE))

  # col denom without by var
  expect_error(
    ard_dichotomous_cell <- ard_dichotomous(svy_dicho,
      by = NULL,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "cell"
    ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_cell, method = FALSE))
})

test_that("ard_dichotomous.survey.design() works with various input types", {
  svy_dicho <- survey::svydesign(ids = ~1, data = mtcars, weights = ~1)

  # logical variables
  # convert variables to logical
  svy_dicho$variables <- svy_dicho$variables |>
    dplyr::mutate(across(c("cyl", "am", "vs"), as.logical))


  # row denom with by var
  expect_error(
    ard_dichotomous_row <-
      ard_dichotomous(svy_dicho,
        by = vs,
        variables = c(cyl, am),
        value = list(cyl = TRUE),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_row, method = FALSE))

  # col denom with by var
  expect_error(
    ard_dichotomous_col <- ard_dichotomous(svy_dicho,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = TRUE),
      denominator = "column"
    ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_col, method = FALSE))

  # cell denom with by var
  expect_error(
    ard_dichotomous_cell <- ard_dichotomous(svy_dicho,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = TRUE),
      denominator = "cell"
    ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_cell, method = FALSE))


  # variables that are neither logical or factor
  svy_dicho <- survey::svydesign(ids = ~1, data = mtcars, weights = ~1)

  svy_dicho$variables <- svy_dicho$variables |>
    dplyr::mutate(across(c("cyl", "am"), as.numeric)) |>
    dplyr::mutate(across("vs", as.character))

  # row denom with by var
  expect_error(
    ard_dichotomous_row <-
      ard_dichotomous(svy_dicho,
        by = vs,
        variables = c(cyl, am),
        value = list(cyl = 4),
        denominator = "row"
      ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_row, method = FALSE))

  # col denom with by var
  expect_error(
    ard_dichotomous_col <- ard_dichotomous(svy_dicho,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "column"
    ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_col, method = FALSE))

  # cell denom with by var
  expect_error(
    ard_dichotomous_cell <- ard_dichotomous(svy_dicho,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "cell"
    ),
    NA
  )
  expect_invisible(cards::check_ard_structure(ard_dichotomous_cell, method = FALSE))
})


test_that("ard_dichotomous.survey.design() returns an error with erroneous input", {
  # value passed in is not logical should return an error
  svy_dicho <- survey::svydesign(ids = ~1, data = mtcars, weights = ~1)

  svy_dicho$variables <- svy_dicho$variables |>
    dplyr::mutate(across(c("cyl", "am"), as.logical))

  expect_snapshot(
    ard_dichotomous(svy_dicho,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "row"
    ),
    error = TRUE
  )

  # supplied factor value is not a level
  svy_dicho$variables <- svy_dicho$variables |>
    dplyr::mutate(across("vs", as.factor))

  svy_dicho$variables$vs
  expect_snapshot(
    ard_dichotomous(svy_dicho,
      by = cyl,
      variables = c(vs, am),
      value = list(vs = 4),
      denominator = "row"
    ),
    error = TRUE
  )

  svy_dicho$variables <- svy_dicho$variables |>
    dplyr::mutate(across("disp", as.numeric))

  expect_snapshot(
    ard_dichotomous(svy_dicho,
      by = cyl,
      variables = c(vs, disp),
      value = list(disp = "turn"),
      denominator = "row"
    ),
    error = TRUE
  )
})


test_that("ard_dichotomous.survey.design() follows ard structure", {
  svy_dicho <- survey::svydesign(ids = ~1, data = mtcars, weights = ~1)
  svy_dicho$variables <- svy_dicho$variables |>
    dplyr::mutate(across(c("cyl", "am", "vs"), as.factor))
  expect_silent(
    ard_dichotomous(svy_dicho,
      by = vs,
      variables = c(cyl, am),
      value = list(cyl = 4),
      denominator = "row"
    ) |>
      cards::check_ard_structure(method = FALSE)
  )
})
