test_that("ard_categorical() univariate", {
  expect_error(
    ard_cat_uni <- ard_categorical(mtcars, variables = "am"),
    NA
  )

  expect_equal(
    ard_cat_uni |>
      flatten_ard() |>
      dplyr::filter(stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      as.integer(),
    table(mtcars$am) |> as.integer()
  )

  expect_equal(
    ard_cat_uni |>
      flatten_ard() |>
      dplyr::filter(stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      as.numeric(),
    table(mtcars$am) |> prop.table() |> as.numeric()
  )

  expect_equal(
    ard_cat_uni |> dplyr::filter(stat_name %in% "N") |> dplyr::pull(statistic) |> unlist(),
    sum(!is.na(mtcars$am))
  )

  expect_equal(
    ard_cat_uni |> dplyr::filter(stat_name %in% "length") |> dplyr::pull(statistic) |> unlist(),
    length(mtcars$am)
  )
})

test_that("ard_categorical() univariate & specified denomiator", {
  expect_error(
    ard_cat_new_denom <-
      ard_categorical(
        mtcars,
        variables = "am",
        denominator = list(mtcars) |> rep_len(100) |> dplyr::bind_rows()
      ),
    NA
  )

  expect_equal(
    ard_cat_new_denom |>
      flatten_ard() |>
      dplyr::filter(stat_name %in% "n") |>
      dplyr::pull(statistic) |>
      as.integer(),
    table(mtcars$am) |> as.integer()
  )

  expect_equal(
    ard_cat_new_denom |>
      flatten_ard() |>
      dplyr::filter(stat_name %in% "p") |>
      dplyr::pull(statistic) |>
      as.numeric(),
      table(mtcars$am) |> prop.table() |> as.numeric() %>% `/`(100)
  )

  expect_equal(
    ard_cat_new_denom |> dplyr::filter(stat_name %in% "N") |> dplyr::pull(statistic) |> unlist(),
    sum(!is.na(mtcars$am)) * 100L
  )

  expect_equal(
    ard_cat_new_denom |> dplyr::filter(stat_name %in% "length") |> dplyr::pull(statistic) |> unlist(),
    length(mtcars$am) * 100L
  )
})
