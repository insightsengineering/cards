skip_if_not(is_pkg_installed("smd", reference_pkg = "cardx"))

test_that("ard_smd_smd() works", {
  expect_error(
    ard_smd <-
      mtcars |>
      ard_smd_smd(by = vs, variables = am, std.error = TRUE),
    NA
  )

  expect_equal(
    ard_smd |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "std.error")),
    smd::smd(x = mtcars$am, g = mtcars$vs, std.error = TRUE) |>
      dplyr::select(-term) |>
      unclass(),
    ignore_attr = TRUE
  )

  # test that the function works with multiple variables at once
  expect_equal(
    dplyr::bind_rows(
      ard_smd,
      mtcars |>
        ard_smd_smd(by = vs, variables = gear, std.error = TRUE)
    ),
    mtcars |>
      ard_smd_smd(by = vs, variables = c(am, gear), std.error = TRUE)
  )
})

test_that("ard_smd() works with survey data", {
  skip_if_not(is_pkg_installed("survey", reference_pkg = "cardx"))

  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

  expect_error(
    ard_smd <-
      dclus1 |>
      ard_smd_smd(by = both, variable = api00, std.error = TRUE),
    NA
  )

  expect_equal(
    ard_smd |>
      cards::get_ard_statistics(stat_name %in% c("estimate", "std.error")),
    smd::smd(x = apiclus1$api00, g = apiclus1$both, w = weights(dclus1), std.error = TRUE) |>
      dplyr::select(-term) |>
      unclass(),
    ignore_attr = TRUE
  )
})

test_that("ard_smd_smd() error messaging", {
  # mis-specify the gref argument
  expect_error(
    bad_gref <-
      ard_smd_smd(cards::ADSL, by = SEX, variables = AGE, std.error = TRUE, gref = 0) |>
      as.data.frame(),
    NA
  )
  # check all the stats still appear despite the errors
  expect_equal(nrow(bad_gref), 3L)
  expect_setequal(bad_gref$stat_name, c("estimate", "std.error", "gref"))
  # check the error message it the one we expect
  expect_equal(
    bad_gref$error |> unique() |> cli::ansi_strip(),
    "gref must be an integer within 2"
  )
})

test_that("ard_smd_smd() follows ard structure", {
  expect_silent(
    mtcars |>
      ard_smd_smd(by = vs, variables = am, std.error = TRUE) |>
      cards::check_ard_structure()
  )
})
