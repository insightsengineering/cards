test_that(".warn_or_error errors/warns depending on input", {
  expect_message(
    .message_or_error("something", FALSE)
  )
  expect_error(
    .message_or_error("something", TRUE)
  )
})


