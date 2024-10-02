skip_if_not(is_pkg_installed(c("survey", "broom"), reference_pkg = "cardx"))

test_that("ard_survey_svyttest() works", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)

  expect_error(
    ard_svyttest <-
      ard_survey_svyttest(
        dclus2,
        variable = enroll,
        by = comp.imp,
        conf.level = 0.9
      ),
    NA
  )

  expect_equal(
    cards::get_ard_statistics(
      ard_svyttest,
      stat_name %in% c("estimate", "p.value")
    ),
    survey::svyttest(enroll ~ comp.imp, dclus2)[c("estimate", "p.value")],
    ignore_attr = TRUE
  )

  expect_equal(
    cards::get_ard_statistics(
      ard_svyttest,
      stat_name %in% c("conf.low", "conf.high")
    ),
    survey::svyttest(enroll ~ comp.imp, dclus2) |>
      confint(level = 0.9) |>
      as.list(),
    ignore_attr = TRUE
  )

  # check that is works with multiple variables
  expect_equal(
    dplyr::bind_rows(
      ard_svyttest,
      ard_survey_svyttest(
        dclus2,
        variable = mobility,
        by = comp.imp,
        conf.level = 0.9
      )
    ),
    ard_survey_svyttest(
      dclus2,
      variable = c(enroll, mobility),
      by = comp.imp,
      conf.level = 0.9
    )
  )
})

test_that("ard_survey_svyttest() messaging", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)

  expect_error(
    ard_svyttest <-
      ard_survey_svyttest(
        dclus2,
        variable = enroll,
        by = stype
      ),
    NA
  )
  expect_equal(
    ard_svyttest$error |> unique() |> unlist(),
    "group must be binary"
  )
})

test_that("ard_survey_svyttest() follows ard structure", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)
  expect_silent(
    ard_survey_svyttest(
      dclus2,
      variable = enroll,
      by = comp.imp,
      conf.level = 0.9
    ) |>
      cards::check_ard_structure()
  )
})
