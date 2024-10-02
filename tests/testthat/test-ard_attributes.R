skip_if_not(is_pkg_installed("withr"))

test_that("ard_attributes() works", {
  withr::local_options(list(width = 120))
  expect_snapshot({
    df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
    attr(df$var1, "label") <- "Lowercase Letters"

    ard_attributes(df, variables = everything(), label = list(var2 = "UPPERCASE LETTERS")) |>
      as.data.frame()
  })
})

test_that("ard_attributes() errors when there is no dataframe", {
  expect_error(
    ard_attributes("test"),
    "There is no method for objects of class <character>."
  )
})

test_that("ard_attributes() follows ard structure", {
  expect_silent(
    ard_attributes(ADSL[c("AGE", "AGEGR1")]) |>
      check_ard_structure(method = FALSE)
  )
})

test_that("ard_attributes() requires label as a named list", {
  expect_snapshot(
    error = TRUE,
    ard_attributes(ADSL[c("AGE", "AGEGR1")],
      label = list("test")
    )
  )
})
