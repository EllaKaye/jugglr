test_that("jugglinglab_url signals a message for ignored args", {
  expect_message(
    jugglinglab_url("3", startpaused = 1),
    "not supported"
  )
  expect_message(
    jugglinglab_url("3", view = "JML"),
    "not supported"
  )
})

test_that("jugglinglab_url does not include ignored args in URL", {
  url <- suppressMessages(jugglinglab_url("3", startpaused = 1))
  expect_false(grepl("startpaused", url))
})
