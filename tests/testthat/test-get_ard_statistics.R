test_that("get_ard_statistics() works", {
  ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")

  expect_snapshot(
    get_ard_statistics(
      ard,
      group1_level %in% "Placebo",
      variable_level %in% "65-80"
    )
  )

  expect_snapshot(
    get_ard_statistics(
      ard,
      group1_level %in% "Placebo",
      variable_level %in% "65-80",
      .attributes = c("warning", "error")
    )
  )
})
