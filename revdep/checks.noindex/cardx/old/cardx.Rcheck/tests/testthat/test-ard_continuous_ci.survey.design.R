skip_if_not(is_pkg_installed("survey", reference_pkg = "cards"))

data(api, package = "survey")
dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

test_that("ard_continuous_ci(data)", {
  expect_snapshot(
    ard_continuous_ci(dclus1, variables = c(api00, api99)) |>
      as.data.frame() |>
      dplyr::select(-warning, -error)
  )
})

test_that("ard_continuous_ci(variables)", {
  expect_silent(
    ard <- ard_continuous_ci(dclus1, variables = c(api00, api99))
  )

  expect_equal(
    cards::get_ard_statistics(ard, variable %in% "api00", stat_name %in% c("estimate", "std.error")),
    survey::svymean(~api00, design = dclus1) |>
      as.data.frame() |>
      as.list() |>
      set_names(c("estimate", "std.error"))
  )

  expect_equal(
    cards::get_ard_statistics(ard, variable %in% "api00", stat_name %in% c("conf.low", "conf.high")),
    survey::svymean(~api00, design = dclus1) |>
      confint(level = 0.95, df = survey::degf(dclus1)) |>
      as.data.frame() |>
      as.list() |>
      set_names(c("conf.low", "conf.high"))
  )

  expect_equal(
    ard_continuous_ci(dclus1, variables = starts_with("xxxxxx")),
    dplyr::tibble()
  )

  # check NA values don't affect result
  dclus1_with_na <- dclus1
  dclus1_with_na$variables[["api00"]][1:100] <- NA
  expect_equal(
    ard_continuous_ci(dclus1_with_na, variables = api00),
    dclus1_with_na |>
      subset(!is.na(api00)) |>
      ard_continuous_ci(variables = api00, df = survey::degf(dclus1_with_na))
  )
})

test_that("ard_continuous_ci(by)", {
  expect_silent(
    ard <- ard_continuous_ci(dclus1, variables = c(api00, api99), by = sch.wide)
  )

  expect_equal(
    cards::get_ard_statistics(ard, group1_level %in% "No", variable %in% "api00", stat_name %in% c("estimate", "std.error")),
    survey::svymean(~api00, design = dclus1 |> subset(sch.wide == "No")) |>
      as.data.frame() |>
      as.list() |>
      set_names(c("estimate", "std.error"))
  )

  expect_equal(
    cards::get_ard_statistics(ard, group1_level %in% "No", variable %in% "api00", stat_name %in% c("conf.low", "conf.high")),
    survey::svymean(~api00, design = dclus1 |> subset(sch.wide == "No")) |>
      confint(level = 0.95, df = survey::degf(dclus1)) |>
      as.data.frame() |>
      as.list() |>
      set_names(c("conf.low", "conf.high"))
  )

  # check that by variables of different classes still work
  expect_equal(
    ard$stat,
    {
      dclus1_copy <- dclus1
      dclus1_copy$variables$sch.wide <- dclus1_copy$variables$sch.wide |> as.integer()
      ard_continuous_ci(dclus1_copy, variables = c(api00, api99), by = sch.wide) |> dplyr::pull("stat")
    }
  )

  expect_equal(
    ard$stat,
    {
      dclus1_copy <- dclus1
      dclus1_copy$variables$sch.wide <- dclus1_copy$variables$sch.wide |> as.character()
      ard_continuous_ci(dclus1_copy, variables = c(api00, api99), by = sch.wide) |> dplyr::pull("stat")
    }
  )
})

test_that("ard_continuous_ci(conf.level)", {
  expect_silent(
    ard <- ard_continuous_ci(dclus1, variables = c(api00, api99), conf.level = 0.80)
  )

  expect_equal(
    cards::get_ard_statistics(ard, variable %in% "api00", stat_name %in% c("conf.low", "conf.high")),
    survey::svymean(~api00, design = dclus1) |>
      confint(level = 0.80, df = survey::degf(dclus1)) |>
      as.data.frame() |>
      as.list() |>
      set_names(c("conf.low", "conf.high"))
  )
})

test_that("ard_continuous_ci(method,variables)", {
  expect_silent(
    ard <- ard_continuous_ci(dclus1, variables = c(api00, api99), method = "svymedian.beta")
  )

  expect_equal(
    cards::get_ard_statistics(ard, variable %in% "api00", stat_name %in% c("estimate", "std.error")),
    survey::svyquantile(~api00, design = dclus1, quantiles = 0.5, interval.type = "beta") |>
      getElement(1L) |>
      as.data.frame() |>
      dplyr::select(quantile, se) |>
      as.list() |>
      set_names(c("estimate", "std.error"))
  )

  expect_equal(
    cards::get_ard_statistics(ard, variable %in% "api00", stat_name %in% c("conf.low", "conf.high")),
    survey::svyquantile(~api00, design = dclus1, quantiles = 0.5, interval.type = "beta") |>
      confint(level = 0.95) |>
      as.data.frame() |>
      as.list() |>
      set_names(c("conf.low", "conf.high"))
  )
})

test_that("ard_continuous_ci(method,by)", {
  expect_silent(
    ard <- ard_continuous_ci(dclus1, variables = c(api00, api99), by = sch.wide, method = "svymedian.beta")
  )

  expect_equal(
    cards::get_ard_statistics(ard, group1_level %in% "No", variable %in% "api00", stat_name %in% c("estimate", "std.error")),
    survey::svyquantile(~api00, design = dclus1 |> subset(sch.wide == "No"), quantiles = 0.5, interval.type = "beta") |>
      getElement(1L) |>
      as.data.frame() |>
      dplyr::select(quantile, se) |>
      as.list() |>
      set_names(c("estimate", "std.error"))
  )

  expect_equal(
    cards::get_ard_statistics(ard, group1_level %in% "No", variable %in% "api00", stat_name %in% c("conf.low", "conf.high")),
    survey::svyquantile(~api00, design = dclus1 |> subset(sch.wide == "No"), quantiles = 0.5, interval.type = "beta") |>
      confint(level = 0.95) |>
      as.data.frame() |>
      as.list() |>
      set_names(c("conf.low", "conf.high"))
  )
})


test_that("ard_continuous_ci(...)", {
  # pass the df argument to `confint()`
  expect_silent(
    ard_svymean <-
      ard_continuous_ci(dclus1, variables = c(api00, api99), df = 50)
  )
  expect_silent(
    ard_svyquantile <-
      ard_continuous_ci(dclus1, variables = c(api00, api99), method = "svymedian.beta", df = 50)
  )

  expect_equal(
    cards::get_ard_statistics(ard_svymean, variable %in% "api00", stat_name %in% c("conf.low", "conf.high")),
    survey::svymean(~api00, design = dclus1) |>
      confint(level = 0.95, df = 50) |>
      as.data.frame() |>
      as.list() |>
      set_names(c("conf.low", "conf.high"))
  )

  expect_equal(
    cards::get_ard_statistics(ard_svyquantile, variable %in% "api00", stat_name %in% c("conf.low", "conf.high")),
    survey::svyquantile(~api00, design = dclus1, quantiles = 0.5, interval.type = "beta") |>
      confint(level = 0.95, df = 50) |>
      as.data.frame() |>
      as.list() |>
      set_names(c("conf.low", "conf.high"))
  )
})

test_that("ard_continuous_ci() errors are captured", {
  expect_snapshot(
    ard_continuous_ci(dclus1, variables = c(api00, api99), df = letters)
  )
  expect_snapshot(
    ard_continuous_ci(dclus1, variables = sch.wide, method = "svymedian.beta")
  )
})
