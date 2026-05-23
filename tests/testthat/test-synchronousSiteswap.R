ss <- synchronousSiteswap(sequence = "(4,2x)*")

test_that("ladder returns a ggplot for synchronousSiteswap", {
  expect_s3_class(ladder(ss), "ggplot")
})

test_that("ladder works with vertical direction", {
  expect_s3_class(ladder(ss, direction = "vertical"), "ggplot")
})

test_that("ladder works with n_cycles argument", {
  expect_s3_class(ladder(ss, n_cycles = 2), "ggplot")
})
