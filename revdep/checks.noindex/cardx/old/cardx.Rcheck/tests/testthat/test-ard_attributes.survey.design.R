skip_if_not(is_pkg_installed(c("survey", "withr")))

test_that("ard_attributes.survey.design() works", {
  withr::local_options(list(width = 120))
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_snapshot({
    attr(dclus1$variables$sname, "label") <- "School Name"

    ard_attributes(
      dclus1,
      variables = c(sname, dname),
      label = list(dname = "District Name")
    ) |>
      as.data.frame()
  })
})
