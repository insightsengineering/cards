test_that("round5() works", {
  expect_snapshot({
    x <- seq.int(-10L, 10L, by = 1L) / 2
    x <- x[x %% 1 != 0] # remove integers
    round5(x) |> setNames(nm = x)
  })

  expect_snapshot({
    x <- seq.int(-100000L, 100000L, by = 10000L) - 1L / 2L
    x <- x[x %% 1 != 0] # remove integers
    round5(x) |> setNames(nm = x)
  })

  expect_snapshot({
    x <- seq.int(-100000L, 100000L, by = 10000L) + 1L / 2L
    x <- x[x %% 1 != 0] # remove integers
    round5(x) |> setNames(nm = x)
  })
})
