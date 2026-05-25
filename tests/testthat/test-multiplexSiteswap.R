ms <- multiplexSiteswap("[43]1")
ms_33 <- multiplexSiteswap("[33]")

# Properties -----------------------------------------------------------------

test_that("multiplexSiteswap has correct type", {
  expect_equal(ms@type, "multiplex")
})

test_that("multiplexSiteswap stores the sequence", {
  expect_equal(ms@sequence, "[43]1")
})

test_that("multiplexSiteswap computes slots", {
  expect_equal(ms@slots, list(c(4L, 3L), c(1L)))
  expect_equal(ms_33@slots, list(c(3L, 3L)))
})

test_that("multiplexSiteswap computes throws", {
  expect_equal(ms@throws, c(4L, 3L, 1L))
  expect_equal(ms_33@throws, c(3L, 3L))
})

test_that("multiplexSiteswap computes period", {
  expect_equal(ms@period, 2L)
  expect_equal(ms_33@period, 1L)
})

test_that("multiplexSiteswap computes symmetry", {
  expect_equal(ms@symmetry, "asymmetrical") # period 2 → even
  expect_equal(ms_33@symmetry, "symmetrical") # period 1 → odd
  expect_equal(multiplexSiteswap("[32]1")@symmetry, "asymmetrical")
})

test_that("multiplexSiteswap computes n_props", {
  expect_equal(ms@n_props, 4) # (4+3+1)/2
  expect_equal(ms_33@n_props, 6) # (3+3)/1
})

test_that("multiplexSiteswap valid is TRUE for valid patterns", {
  expect_true(ms@valid)
  expect_true(ms_33@valid)
  expect_true(multiplexSiteswap("[32]1")@valid)
  expect_true(multiplexSiteswap("[54]1")@valid)
})

test_that("multiplexSiteswap valid is FALSE for invalid patterns", {
  # [53]4: n_props=6 but landing counts don't match throw counts
  expect_false(multiplexSiteswap("[53]4")@valid)
  # [42]1: n_props = (4+2+1)/2 = 3.5, not a whole number
  expect_false(multiplexSiteswap("[42]1")@valid)
})

# Validation -----------------------------------------------------------------

test_that("multiplexSiteswap rejects non-multiplex notation", {
  expect_error(multiplexSiteswap("531"))
  expect_error(multiplexSiteswap("(4,2x)*"))
  expect_error(multiplexSiteswap("[43"))
  expect_error(multiplexSiteswap("[]"))
})

# throw_data -----------------------------------------------------------------

test_that("throw_data returns a data frame for multiplexSiteswap", {
  expect_s3_class(throw_data(ms), "data.frame")
})

test_that("throw_data has expected columns for multiplexSiteswap", {
  expect_named(
    throw_data(ms),
    c("beat", "hand", "throw", "catch_beat", "catch_hand", "prop")
  )
})

test_that("throw_data has one row per individual throw across n_cycles", {
  # [43]1 has 3 individual throws per cycle; 3 cycles = 9 rows
  expect_equal(nrow(throw_data(ms, n_cycles = 3)), 9L)
  expect_equal(nrow(throw_data(ms, n_cycles = 1)), 3L)
  # [33] has 2 throws in 1 slot; 3 cycles = 6 rows
  expect_equal(nrow(throw_data(ms_33, n_cycles = 3)), 6L)
})

test_that("throw_data assigns a prop to every row", {
  expect_false(anyNA(throw_data(ms)$prop))
})

test_that("throw_data errors for invalid n_cycles", {
  expect_error(
    throw_data(ms, n_cycles = 1.3),
    class = "jugglr_error_bad_n_cycles"
  )
  expect_error(
    throw_data(ms, n_cycles = 0),
    class = "jugglr_error_bad_n_cycles"
  )
})

# timeline -------------------------------------------------------------------

test_that("timeline returns a ggplot for multiplexSiteswap", {
  expect_s3_class(timeline(ms), "ggplot")
  expect_s3_class(timeline(ms_33), "ggplot")
})

test_that("timeline works with title = FALSE", {
  expect_s3_class(timeline(ms, title = FALSE), "ggplot")
})

test_that("timeline works with n_cycles argument", {
  expect_s3_class(timeline(ms, n_cycles = 2), "ggplot")
})

# ladder ---------------------------------------------------------------------

test_that("ladder returns a ggplot for multiplexSiteswap", {
  expect_s3_class(ladder(ms), "ggplot")
  expect_s3_class(ladder(ms_33), "ggplot")
})

test_that("ladder works with vertical direction", {
  expect_s3_class(ladder(ms, direction = "vertical"), "ggplot")
})

test_that("ladder works with n_cycles argument", {
  expect_s3_class(ladder(ms, n_cycles = 2), "ggplot")
})
