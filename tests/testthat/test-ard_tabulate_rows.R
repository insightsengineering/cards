test_that("ard_tabulate_rows() works", {
  expect_snapshot(
    ard_tabulate_rows(ADSL, by = TRTA)
  )
})
