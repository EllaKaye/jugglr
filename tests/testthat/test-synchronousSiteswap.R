ss <- synchronousSiteswap(sequence = "(4,2x)*")
ss_44 <- synchronousSiteswap(sequence = "(4,4)")

# Properties -----------------------------------------------------------------

test_that("synchronousSiteswap has correct type", {
  expect_equal(ss@type, "synchronous")
})

test_that("synchronousSiteswap stores the sequence", {
  expect_equal(ss@sequence, "(4,2x)*")
})

test_that("synchronousSiteswap computes full_sequence", {
  expect_equal(ss@full_sequence, "(4,2x)(2x,4)")
  expect_equal(ss_44@full_sequence, "(4,4)")
})

test_that("synchronousSiteswap computes throws", {
  expect_equal(ss@throws, c("4", "2x", "2x", "4"))
})

test_that("synchronousSiteswap computes throws_by_hand", {
  expect_equal(
    ss@throws_by_hand,
    list(hand_1 = c("4", "2x"), hand_2 = c("2x", "4"))
  )
})

test_that("synchronousSiteswap computes period", {
  expect_equal(ss@period, 4L)
  expect_equal(ss_44@period, 2L)
})

test_that("synchronousSiteswap computes symmetry", {
  expect_equal(ss@symmetry, "symmetrical")
  expect_equal(synchronousSiteswap("(6x,4)(4,2x)")@symmetry, "asymmetrical")
})

test_that("synchronousSiteswap computes n_props", {
  expect_equal(ss@n_props, 3)
  expect_equal(ss_44@n_props, 4)
})

test_that("synchronousSiteswap valid is TRUE for valid pattern", {
  expect_true(ss@valid)
  expect_true(ss_44@valid)
})

test_that("synchronousSiteswap valid is FALSE for invalid pattern", {
  expect_false(synchronousSiteswap("(6,2)(4x,6x)")@valid)
})

# Validation -----------------------------------------------------------------

test_that("synchronousSiteswap rejects non-sync notation", {
  expect_error(synchronousSiteswap("531"))
  expect_error(synchronousSiteswap("(4,4,4)"))
})

test_that("synchronousSiteswap rejects odd throws", {
  expect_error(synchronousSiteswap("(4,3)"))
  expect_error(synchronousSiteswap("(3,3)"))
})

# print ----------------------------------------------------------------------

test_that("print shows valid message for synchronousSiteswap", {
  expect_message(print(ss), "valid synchronous siteswap")
})

test_that("print shows full_sequence when * notation is used", {
  expect_message(print(ss), "Full sequence")
})

test_that("print omits full_sequence when no * notation", {
  msgs <- capture.output(print(ss_44), type = "message")
  expect_false(any(grepl("Full sequence", msgs)))
})

# throw_data -----------------------------------------------------------------

test_that("throw_data returns a data frame for synchronousSiteswap", {
  expect_s3_class(throw_data(ss), "data.frame")
})

test_that("throw_data has expected columns for synchronousSiteswap", {
  expect_named(
    throw_data(ss),
    c(
      "beat",
      "hand",
      "throw",
      "is_crossing",
      "catch_beat",
      "catch_hand",
      "prop"
    )
  )
})

test_that("throw_data has 2 * n_slots * n_cycles rows", {
  expect_equal(nrow(throw_data(ss, n_cycles = 2)), 8L)
})

test_that("throw_data errors for invalid n_cycles", {
  expect_error(
    throw_data(ss, n_cycles = 1.3),
    class = "jugglr_error_bad_n_cycles"
  )
  expect_error(
    throw_data(ss, n_cycles = 0),
    class = "jugglr_error_bad_n_cycles"
  )
})

# ladder ---------------------------------------------------------------------

test_that("ladder returns a ggplot for synchronousSiteswap", {
  expect_s3_class(ladder(ss), "ggplot")
  expect_s3_class(ladder(ss_44), "ggplot")
})

test_that("ladder works with vertical direction", {
  expect_s3_class(ladder(ss, direction = "vertical"), "ggplot")
})

test_that("ladder works with n_cycles argument", {
  expect_s3_class(ladder(ss, n_cycles = 2), "ggplot")
})

test_that("ladder respects title = FALSE", {
  p <- ladder(ss, title = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
})

test_that("ladder respects subtitle = FALSE", {
  p <- ladder(ss, subtitle = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$subtitle)
})

# timeline -------------------------------------------------------------------

test_that("timeline returns a ggplot for synchronousSiteswap", {
  expect_s3_class(timeline(ss), "ggplot")
  expect_s3_class(timeline(ss_44), "ggplot")
})

test_that("timeline respects title = FALSE", {
  p <- timeline(ss, title = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
})

test_that("timeline respects subtitle = FALSE", {
  p <- timeline(ss, subtitle = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$subtitle)
})
