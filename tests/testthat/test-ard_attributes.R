test_that("ard_attributes() works", {
  expect_snapshot({
    df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
    attr(df$var1, "label") <- "Lowercase Letters"

    ard_attributes(df, variables = everything(), label = list(var2 = "UPPERCASE LETTERS")) |>
      as.data.frame()
  })
})
