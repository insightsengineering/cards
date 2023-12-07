test_that("ard_continuous() labels", {

  # formula
  ard_test <- ard_continuous(data = ADSL,
                             by = "ARM",
                             variables = c("AGE","BMIBL"),
                             stat_labels = everything() ~ list(c("min","max") ~ "min - max"))

  ard_test_labs <- ard_test %>%
    dplyr::filter(stat_name %in% c("min","max")) |>
    dplyr::pull(stat_label) |>
    unique()

  expect_equal(ard_test_labs,
               "min - max")

  # list
  ard_test <- ard_continuous(data = ADSL,
                             by = "ARM",
                             variables = c("AGE","BMIBL"),
                             stat_labels = everything() ~ list(p25 = "25th %ile", p75 = "75th %ile"))

  actual_labs <- ard_test %>%
    dplyr::filter(stat_name %in% c("p25","p75")) |>
    dplyr::select(stat_name, stat_label) |>
    dplyr::arrange(stat_name, stat_label) |>
    unique()

  expected_labs <- dplyr::tibble(
    stat_name = c("p25","p75"),
    stat_label = c("25th %ile","75th %ile")
  )

  expect_equal(actual_labs,
              expected_labs,
              ignore_attr = "class")

  # variable-specific
  ard_test <- ard_continuous(data = ADSL,
                             by = "ARM",
                             variables = c("AGE","BMIBL"),
                             stat_labels = AGE ~ list(p25 = "25th %ile", p75 = "75th %ile"))

  actual_labs <- ard_test %>%
    dplyr::filter(stat_name %in% c("p25","p75")) |>
    dplyr::select(variable, stat_name, stat_label) |>
    dplyr::arrange(variable, stat_name, stat_label) |>
    unique()

  expected_labs <- dplyr::tribble(
    ~ variable  ,  ~stat_name, ~stat_label,
    "AGE"       , "p25"        , "25th %ile"  ,
    "AGE"       , "p75"        , "75th %ile"     ,
    "BMIBL"     , "p25"        , "25th Percentile",
    "BMIBL"     , "p75"        , "75th Percentile"
  )

  expect_equal(actual_labs, expected_labs, ignore_attr = "class")
})


test_that("ard_categorical() labels", {


  # formula
  ard_test <- ard_categorical(data = ADSL,
                             by = "ARM",
                             variables = c("AGEGR1","SEX"),
                             stat_labels = everything() ~ list(c("n","p") ~ "n (pct)"))

  actual_labs <- ard_test %>%
                 dplyr::filter(stat_name %in% c("n","p")) |>
                 dplyr::select(stat_name, stat_label) |>
                 dplyr::arrange(stat_name, stat_label) |>
                 unique()

  expected_labs <- dplyr::tibble(
                 stat_name = c("n","p"),
                 stat_label = "n (pct)")

  expect_equal(actual_labs, expected_labs, ignore_attr = "class")

  # list
  ard_test <- ard_categorical(data = ADSL,
                              by = "ARM",
                              variables = c("AGEGR1","SEX"),
                              stat_labels = everything() ~ list(n = "num", p = "pct"))

  actual_labs <- ard_test %>%
                 dplyr::filter(stat_name %in% c("n","p")) |>
                 dplyr::select(stat_name, stat_label) |>
                 dplyr::arrange(stat_name, stat_label) |>
                 unique()

  expected_labs <- dplyr::tibble(
                 stat_name = c("n","p"),
                 stat_label = c("num", "pct"))

  expect_equal(actual_labs, expected_labs, ignore_attr = "class")

  # variable-specific
  ard_test <-ard_categorical(data = ADSL,
                             by = "ARM",
                             variables = c("AGEGR1","SEX"),
                             stat_labels = AGEGR1 ~ list(c("n","p") ~ "n (pct)"))


  actual_labs <- ard_test %>%
    dplyr::filter(stat_name %in% c("n","p")) |>
    dplyr::select(variable, stat_name, stat_label) |>
    dplyr::arrange(variable, stat_name, stat_label) |>
    unique()

  expected_labs <- dplyr::tribble(
    ~ variable  ,  ~stat_name, ~stat_label,
    "AGEGR1"    , "n"        , "n (pct)",
    "AGEGR1"    , "p"        , "n (pct)",
    "SEX"       , "n"        , "n",
    "SEX"       , "p"        , "%"
  )

  expect_equal(actual_labs, expected_labs, ignore_attr = "class")

})
