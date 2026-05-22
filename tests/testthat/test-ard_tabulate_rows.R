test_that("ard_tabulate_rows() works", {
  out <- ard_tabulate_rows(ADSL, by = TRTA)
  expect_named(
    out,
    c(
      "group1", "group1_level", "variable", "variable_level", "context",
      "stat_name", "stat_label", "stat", "fmt_fun", "warning", "error"
    )
  )
  expect_all_equal(NROW(out), length(unique(ADSL$TRTA)))

  expect_snapshot(
    ard_tabulate_rows(ADSL, by = TRTA)
  )
})
