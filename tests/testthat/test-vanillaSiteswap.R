s <- vanillaSiteswap("531")

# Properties -----------------------------------------------------------------

test_that("vanillaSiteswap has correct type", {
  expect_equal(s@type, "vanilla")
})

test_that("vanillaSiteswap stores the sequence", {
  expect_equal(s@sequence, "531")
})

test_that("vanillaSiteswap computes throws", {
  expect_equal(s@throws, c(5, 3, 1))
})

test_that("vanillaSiteswap computes period", {
  expect_equal(s@period, 3L)
})

test_that("vanillaSiteswap computes symmetry", {
  expect_equal(s@symmetry, "symmetrical")
  expect_equal(vanillaSiteswap("4444")@symmetry, "asymmetrical")
})

test_that("vanillaSiteswap computes n_props", {
  expect_equal(s@n_props, 3)
})

test_that("vanillaSiteswap computes can_throw", {
  expect_true(s@can_throw)
  expect_false(vanillaSiteswap("432")@can_throw)
})

test_that("vanillaSiteswap computes satisfies_average_theorem", {
  expect_true(s@satisfies_average_theorem)
  expect_false(vanillaSiteswap("43")@satisfies_average_theorem)
})

test_that("vanillaSiteswap valid is TRUE for valid pattern", {
  expect_true(s@valid)
})

test_that("vanillaSiteswap valid is FALSE for invalid pattern", {
  expect_false(vanillaSiteswap("432")@valid)
  expect_false(vanillaSiteswap("43")@valid)
})

# Validation -----------------------------------------------------------------

test_that("vanillaSiteswap rejects non-alphanumeric sequences", {
  expect_error(vanillaSiteswap("(4,4)"))
  expect_error(vanillaSiteswap("5-3-1"))
})

# print ----------------------------------------------------------------------

test_that("print shows valid message for valid pattern", {
  expect_message(print(s), "valid vanilla siteswap")
})

test_that("print shows invalid message for collision", {
  expect_message(print(vanillaSiteswap("432")), "not a valid juggling pattern")
  expect_message(print(vanillaSiteswap("432")), "same beat")
})

test_that("print shows invalid message for average theorem failure", {
  expect_message(print(vanillaSiteswap("43")), "not a valid juggling pattern")
  expect_message(print(vanillaSiteswap("43")), "whole number")
})

test_that("print shows both reasons when both conditions fail", {
  msgs <- capture.output(print(vanillaSiteswap("221")), type = "message")
  expect_true(any(grepl("whole number", msgs)))
  expect_true(any(grepl("same beat", msgs)))
})

# throw_data -----------------------------------------------------------------

test_that("throw_data returns a data frame", {
  expect_s3_class(throw_data(s), "data.frame")
})

test_that("throw_data has expected columns", {
  expect_named(
    throw_data(s),
    c("beat", "hand", "throw", "catch_beat", "catch_hand", "prop")
  )
})

test_that("throw_data has period * n_cycles rows", {
  expect_equal(nrow(throw_data(s, n_cycles = 2)), 6L)
  expect_equal(nrow(throw_data(s, n_cycles = 3)), 9L)
})

test_that("throw_data errors for invalid n_cycles", {
  expect_error(
    throw_data(s, n_cycles = 1.3),
    class = "jugglr_error_bad_n_cycles"
  )
  expect_error(throw_data(s, n_cycles = 0), class = "jugglr_error_bad_n_cycles")
  expect_error(
    throw_data(s, n_cycles = -1),
    class = "jugglr_error_bad_n_cycles"
  )
  expect_error(
    throw_data(s, n_cycles = c(2, 3)),
    class = "jugglr_error_bad_n_cycles"
  )
})

# timeline -------------------------------------------------------------------

test_that("timeline returns a ggplot", {
  expect_s3_class(timeline(s), "ggplot")
})

test_that("timeline works without title", {
  expect_s3_class(timeline(s, title = FALSE), "ggplot")
})

test_that("timeline works with n_cycles", {
  expect_s3_class(timeline(s, n_cycles = 2), "ggplot")
})

# ladder ---------------------------------------------------------------------

test_that("ladder returns a ggplot for vanillaSiteswap", {
  expect_s3_class(ladder(s), "ggplot")
})

test_that("ladder works with vertical direction", {
  expect_s3_class(ladder(s, direction = "vertical"), "ggplot")
})

test_that("ladder works with n_cycles", {
  expect_s3_class(ladder(s, n_cycles = 2), "ggplot")
})
