test_that("multiplication works", {
  expect_error(
    ard_strata <-
      ard_stratified_categorical(
        data = ADAE |> dplyr::filter(AOCCPFL %in% "Y"),
        strata = "AESOC",
        variables = "AEDECOD",
        denominator = ADSL
      ),
    NA
  )
})
