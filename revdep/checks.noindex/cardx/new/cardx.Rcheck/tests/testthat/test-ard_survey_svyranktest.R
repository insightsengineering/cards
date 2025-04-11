skip_if_not(is_pkg_installed(c("survey", "broom")))

test_that("ard_survey_svyranktest() works", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)
  svyranktest <- lapply(
    c("wilcoxon", "vanderWaerden", "median", "KruskalWallis"),
    function(x) {
      ard_survey_svyranktest(
        dclus2,
        variable = enroll,
        by = comp.imp,
        test = x
      )
    }
  )

  expect_snapshot(svyranktest[[1]] |> as.data.frame() |> dplyr::select(stat_label, stat))
  expect_snapshot(svyranktest[[2]] |> as.data.frame() |> dplyr::select(stat_label, stat))
  expect_snapshot(svyranktest[[3]] |> as.data.frame() |> dplyr::select(stat_label, stat))
  expect_snapshot(svyranktest[[4]] |> as.data.frame() |> dplyr::select(stat_label, stat))
})

test_that("exact values match for ard_svyranktest works", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)
  svywilcox <- ard_survey_svyranktest(
    dclus2,
    variable = enroll,
    by = comp.imp,
    test = "wilcoxon"
  )
  expect_equal(
    cards::get_ard_statistics(
      svywilcox,
      stat_name %in% c("estimate", "p.value")
    ),
    survey::svyranktest(enroll ~ comp.imp, dclus2, test = "wilcoxon")[c("estimate", "p.value")],
    ignore_attr = TRUE
  )
})

test_that("ard_survey_svyranktest() follows ard structure", {
  data(api, package = "survey")
  dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)
  expect_silent(
    ard_survey_svyranktest(
      dclus2,
      variable = enroll,
      by = comp.imp,
      test = "wilcoxon"
    ) |>
      cards::check_ard_structure()
  )
})
