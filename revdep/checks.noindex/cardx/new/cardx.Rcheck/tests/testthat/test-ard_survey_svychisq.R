skip_if_not(is_pkg_installed(c("survey", "broom")))

test_that("ard_survey_svychisq() works", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)

  expect_error(
    ard_svychisq <-
      ard_survey_svychisq(
        dclus2,
        variables = sch.wide,
        by = comp.imp,
        statistic = "F"
      ),
    NA
  )

  expect_equal(
    cards::get_ard_statistics(
      ard_svychisq,
      stat_name %in% c("statistic", "p.value")
    ),
    survey::svychisq(~ sch.wide + comp.imp, dclus2)[c("statistic", "p.value")],
    ignore_attr = TRUE
  )

  # test that the function works with multiple variables
  expect_snapshot(
    ard_survey_svychisq(
      dclus2,
      variables = c(sch.wide, stype),
      by = comp.imp,
      statistic = "adjWald"
    ) |>
      dplyr::select(c(1:3, 5:6)) |>
      dplyr::group_by(variable) |>
      dplyr::slice_head(n = 3) |>
      as.data.frame()
  )


  expect_equal(
    dplyr::bind_rows(
      ard_svychisq,
      dclus2 |>
        ard_survey_svychisq(by = comp.imp, variables = stype)
    ),
    dclus2 |>
      ard_survey_svychisq(by = comp.imp, variables = c(sch.wide, stype))
  )

  # works with non-syntactic names
  expect_equal(
    {
      dclus2_syntactic <- dclus2
      dclus2_syntactic$variables <-
        dplyr::rename(dclus2_syntactic$variables, `comp imp` = comp.imp)
      ard_survey_svychisq(
        dclus2,
        variables = sch.wide,
        by = comp.imp,
        statistic = "F"
      )[c("context", "stat_name", "stat_label", "stat")]
    },
    ard_svychisq[c("context", "stat_name", "stat_label", "stat")]
  )
})

test_that("ard_survey_svychisq() follows ard structure", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)
  expect_silent(
    ard_survey_svychisq(
      dclus2,
      variables = sch.wide,
      by = comp.imp,
      statistic = "F"
    ) |>
      cards::check_ard_structure()
  )
})
