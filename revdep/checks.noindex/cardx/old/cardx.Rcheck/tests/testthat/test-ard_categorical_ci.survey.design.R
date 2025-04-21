skip_if_not(is_pkg_installed("survey"))

data(api, package = "survey")
dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

test_that("ard_categorical_ci(data)", {
  expect_snapshot(
    ard_categorical_ci(dclus1, variables = c(both, awards)) |>
      dplyr::select(-warning, -error, -fmt_fn, -context)
  )
})

test_that("ard_categorical_ci(variables)", {
  expect_silent(
    ard <- ard_categorical_ci(dclus1, variables = c(both, awards))
  )

  expect_equal(
    cards::get_ard_statistics(ard, variable %in% "both", map(variable_level, as.character) %in% "No")[c("estimate", "conf.low", "conf.high")],
    survey::svyciprop(~ I(both == "No"), design = dclus1, method = "logit", level = 0.95) %>%
      {c(as.list(.), as.list(attr(., "ci")))} |> # styler: off
      set_names(c("estimate", "conf.low", "conf.high"))
  )

  expect_equal(
    ard_categorical_ci(dclus1, variables = starts_with("xxxxxx")),
    dplyr::tibble() |> cards::as_card()
  )

  # check all works with numeric variable
  dclus1_with_dbl <- dclus1
  dclus1_with_dbl$variables[["both"]] <- dclus1_with_dbl$variables[["both"]] |> as.numeric()
  expect_equal(
    ard_categorical_ci(dclus1_with_dbl, variables = both) |> dplyr::select(-variable_level),
    ard_categorical_ci(dclus1, variables = both) |> dplyr::select(-variable_level)
  )

  # check NA values don't affect result
  dclus1_with_na <- dclus1
  dclus1_with_na$variables[["both"]][1:100] <- NA
  expect_equal(
    ard_categorical_ci(dclus1_with_na, variables = both),
    dclus1_with_na |>
      subset(!is.na(both)) |>
      ard_categorical_ci(variables = both, df = survey::degf(dclus1_with_na))
  )
})


test_that("ard_categorical_ci(by)", {
  expect_silent(
    ard <- ard_categorical_ci(dclus1, variables = c(both, awards), by = sch.wide)
  )

  expect_equal(
    cards::get_ard_statistics(
      ard,
      map(group1_level, as.character) %in% "No",
      variable %in% "both",
      map(variable_level, as.character) %in% "No",
      stat_name %in% c("estimate", "conf.low", "conf.high")
    ),
    survey::svyciprop(~ I(both == "No"), design = dclus1 |> subset(sch.wide == "No")) %>%
      {c(as.list(.), as.list(attr(., "ci")))} |> # styler: off
      set_names(c("estimate", "conf.low", "conf.high"))
  )

  # check that by variables of different classes still work
  expect_equal(
    ard$stat,
    {
      dclus1_copy <- dclus1
      dclus1_copy$variables$sch.wide <- dclus1_copy$variables$sch.wide |> as.integer()
      ard_categorical_ci(dclus1_copy, variables = c(both, awards), by = sch.wide) |> dplyr::pull("stat")
    }
  )

  expect_equal(
    ard$stat,
    {
      dclus1_copy <- dclus1
      dclus1_copy$variables$sch.wide <- dclus1_copy$variables$sch.wide |> as.character()
      ard_categorical_ci(dclus1_copy, variables = c(both, awards), by = sch.wide) |> dplyr::pull("stat")
    }
  )
})

test_that("ard_categorical_ci(conf.level)", {
  expect_silent(
    ard <- ard_categorical_ci(dclus1, variables = c(both, awards), conf.level = 0.80)
  )

  expect_equal(
    cards::get_ard_statistics(ard, variable %in% "both", map(variable_level, as.character) %in% "No", stat_name %in% c("estimate", "conf.low", "conf.high")),
    survey::svyciprop(~ I(both == "No"), design = dclus1, level = 0.80, df = survey::degf(dclus1)) %>%
      {c(as.list(.), as.list(attr(., "ci")))} |> # styler: off
      set_names(c("estimate", "conf.low", "conf.high"))
  )
})

test_that("ard_categorical_ci(method)", {
  expect_silent(
    ard <- ard_categorical_ci(dclus1, variables = c(both, awards), method = "likelihood")
  )

  expect_equal(
    cards::get_ard_statistics(ard, variable %in% "both", map(variable_level, as.character) %in% "No", stat_name %in% c("estimate", "conf.low", "conf.high")),
    survey::svyciprop(~ I(both == "No"), design = dclus1, method = "likelihood", df = survey::degf(dclus1)) %>%
      {c(as.list(.), as.list(attr(., "ci")))} |> # styler: off
      set_names(c("estimate", "conf.low", "conf.high"))
  )

  # check type
  expect_true(ard$variable_level |> unique() |> map_lgl(is.factor) |> all())
})

test_that("ard_categorical_ci(value)", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_equal(
    ard_categorical_ci(dclus1, variables = sch.wide, value = sch.wide ~ "Yes", method = "xlogit"),
    ard_categorical_ci(dclus1, variables = sch.wide, method = "xlogit") |>
      dplyr::filter(unlist(variable_level) %in% "Yes")
  )

  expect_equal(
    ard_categorical_ci(dclus1, variables = c(sch.wide, both), value = list(sch.wide ~ "Yes", both ~ "Yes"), method = "xlogit"),
    ard_categorical_ci(dclus1, variables = c(sch.wide, both), method = "xlogit") |>
      dplyr::filter(map(variable_level, as.character) %in% "Yes")
  )
})

test_that("ard_categorical_ci.survey.design() follows ard structure", {
  expect_silent(
    ard_categorical_ci(dclus1, variables = c(both, awards), method = "likelihood") |>
      cards::check_ard_structure(method = TRUE)
  )
})
