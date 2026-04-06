test_that("register_data() works", {

})

test_that("register_data() in put checks", {
  expect_snapshot(
    register_data(data = letters)
  )
  expect_snapshot(
    register_data(data = ADSL, name = letters)
  )
  expect_snapshot(
    register_data(data = ADSL, name = letters)
  )
})

