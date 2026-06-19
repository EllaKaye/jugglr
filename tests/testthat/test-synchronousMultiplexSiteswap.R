smp <- synchronousMultiplexSiteswap("(2,4)([4x4],2x)")
smp_star <- synchronousMultiplexSiteswap("(4,[42x])*")

# Properties -----------------------------------------------------------------

test_that("synchronousMultiplexSiteswap has correct type", {
  expect_equal(smp@type, "synchronous multiplex")
})

test_that("synchronousMultiplexSiteswap stores the sequence", {
  expect_equal(smp@sequence, "(2,4)([4x4],2x)")
})

test_that("synchronousMultiplexSiteswap computes full_sequence", {
  expect_equal(smp@full_sequence, "(2,4)([4x4],2x)")
  expect_equal(smp_star@full_sequence, "(4,[42x])([42x],4)")
})

test_that("synchronousMultiplexSiteswap computes throws", {
  expect_equal(smp@throws, c("2", "4", "[4x4]", "2x"))
  expect_equal(smp_star@throws, c("4", "[42x]", "[42x]", "4"))
})

test_that("synchronousMultiplexSiteswap computes throws_by_hand", {
  expect_equal(
    smp@throws_by_hand,
    list(hand_1 = c("2", "[4x4]"), hand_2 = c("4", "2x"))
  )
})

test_that("synchronousMultiplexSiteswap computes period", {
  expect_equal(smp@period, 4L)
  expect_equal(smp_star@period, 4L)
})

test_that("synchronousMultiplexSiteswap computes symmetry", {
  expect_equal(smp@symmetry, "asymmetrical")
  expect_equal(smp_star@symmetry, "symmetrical")
})

test_that("synchronousMultiplexSiteswap computes n_props", {
  expect_equal(smp@n_props, 4)
  expect_equal(smp_star@n_props, 5)
})

test_that("synchronousMultiplexSiteswap valid is TRUE for valid patterns", {
  expect_true(smp@valid)
  expect_true(smp_star@valid)
})

test_that("synchronousMultiplexSiteswap valid is FALSE for invalid pattern", {
  expect_false(synchronousMultiplexSiteswap("(2,4)([2x2],2)")@valid)
})

# Validation -----------------------------------------------------------------

test_that("synchronousMultiplexSiteswap rejects non-sync-multiplex notation", {
  expect_error(
    synchronousMultiplexSiteswap("531"),
    class = "jugglr_error_invalid_sequence"
  )
  expect_error(
    synchronousMultiplexSiteswap("(4,2x)"),
    class = "jugglr_error_invalid_sequence"
  )
  expect_error(
    synchronousMultiplexSiteswap("[43]1"),
    class = "jugglr_error_invalid_sequence"
  )
})

test_that("synchronousMultiplexSiteswap rejects odd throws", {
  expect_error(
    synchronousMultiplexSiteswap("(2,4)([3x4],2)"),
    class = "jugglr_error_invalid_sequence"
  )
  expect_error(
    synchronousMultiplexSiteswap("(2,4)([4x4],3)"),
    class = "jugglr_error_invalid_sequence"
  )
})

# Dispatcher -----------------------------------------------------------------

test_that("siteswap() dispatches to synchronousMultiplexSiteswap", {
  expect_s7_class(siteswap("(2,4)([4x4],2x)"), synchronousMultiplexSiteswap)
})

test_that("siteswap() still dispatches plain sync correctly", {
  expect_s7_class(siteswap("(4,4)"), synchronousSiteswap)
})

test_that("siteswap() still dispatches plain multiplex correctly", {
  expect_s7_class(siteswap("[43]1"), multiplexSiteswap)
})

# print ----------------------------------------------------------------------

test_that("print shows valid message for synchronousMultiplexSiteswap", {
  expect_message(print(smp), "valid synchronous multiplex siteswap")
})

test_that("print shows full_sequence when * notation is used", {
  expect_message(print(smp_star), "Full sequence")
})

test_that("print omits full_sequence when no * notation", {
  msgs <- capture.output(print(smp), type = "message")
  expect_false(any(grepl("Full sequence", msgs)))
})

# throw_data -----------------------------------------------------------------

test_that("throw_data returns a data frame for synchronousMultiplexSiteswap", {
  expect_s3_class(throw_data(smp), "data.frame")
})

test_that("throw_data has expected columns for synchronousMultiplexSiteswap", {
  expect_named(
    throw_data(smp),
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

test_that("throw_data has correct row count (one row per individual throw)", {
  # smp has 2 sync slots * 3 cycles = 6 beats; slot 2 hand 0 throws 2 props
  # per cycle: 5 throws (2 + 4 + [4x4]=2 + 2x = 5), so 5*3 = 15 rows
  expect_equal(nrow(throw_data(smp, n_cycles = 1)), 5L)
  expect_equal(nrow(throw_data(smp, n_cycles = 3)), 15L)
})

test_that("throw_data uses even beats and catch beats", {
  td <- throw_data(smp)
  expect_true(all(td$beat %% 2 == 0))
  expect_true(all(td$catch_beat %% 2 == 0))
  expect_equal(min(td$beat), 0)
})

test_that("throw_data errors for invalid n_cycles", {
  expect_error(
    throw_data(smp, n_cycles = 1.3),
    class = "jugglr_error_bad_n_cycles"
  )
})

# ladder ---------------------------------------------------------------------

test_that("ladder returns a ggplot for synchronousMultiplexSiteswap", {
  expect_s3_class(ladder(smp), "ggplot")
  expect_s3_class(ladder(smp_star), "ggplot")
})

test_that("ladder works with vertical direction", {
  expect_s3_class(ladder(smp, direction = "vertical"), "ggplot")
})

test_that("ladder respects title = FALSE", {
  p <- ladder(smp, title = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
})

test_that("ladder respects subtitle = FALSE", {
  p <- ladder(smp, subtitle = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$subtitle)
})

# timeline -------------------------------------------------------------------

test_that("timeline returns a ggplot for synchronousMultiplexSiteswap", {
  expect_s3_class(timeline(smp), "ggplot")
  expect_s3_class(timeline(smp_star), "ggplot")
})

test_that("timeline respects title = FALSE", {
  p <- timeline(smp, title = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
})

test_that("timeline respects subtitle = FALSE", {
  p <- timeline(smp, subtitle = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$subtitle)
})

test_that("timeline places the two hands on opposite sides of the centre line", {
  p <- timeline(smp)
  pd <- ggplot2::layer_data(p, length(p$layers))
  expect_lt(min(pd$y), 0)
  expect_gt(max(pd$y), 0)
})
