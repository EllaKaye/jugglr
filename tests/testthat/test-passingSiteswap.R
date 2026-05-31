s6 <- passingSiteswap("<3p 3 3 3 3 3 | 3p 3 3 3 3 3>") # 6-count, 6 props
s7 <- passingSiteswap("<4p 3 | 3 4p>") # 7-club 2-count, valid
s_invalid <- passingSiteswap("<4p 3 | 4p 3>") # collision
sf <- passingSiteswap("<4.5 3 3 | 3 4 3.5>") # fractional, 7 props
s3 <- passingSiteswap("<3p 3 3 | 3p 3 3 | 3p 3 3>") # 3-juggler p-notation

# Properties -----------------------------------------------------------------

test_that("passingSiteswap has correct type", {
  expect_equal(s6@type, "passing")
})

test_that("passingSiteswap stores the sequence", {
  expect_equal(s6@sequence, "<3p 3 3 3 3 3 | 3p 3 3 3 3 3>")
})

test_that("passingSiteswap detects fractional notation", {
  expect_false(s6@is_fractional)
  expect_false(s7@is_fractional)
  expect_true(sf@is_fractional)
})

test_that("passingSiteswap computes n_jugglers", {
  expect_equal(s6@n_jugglers, 2L)
  expect_equal(s7@n_jugglers, 2L)
  expect_equal(sf@n_jugglers, 2L)
})

test_that("passingSiteswap computes sequences_by_juggler", {
  expect_equal(s7@sequences_by_juggler, list(c("4p", "3"), c("3", "4p")))
})

test_that("passingSiteswap computes throws_by_juggler", {
  expect_equal(s7@throws_by_juggler, list(c(4, 3), c(3, 4)))
})

test_that("passingSiteswap computes is_pass_by_juggler", {
  expect_equal(s7@is_pass_by_juggler, list(c(TRUE, FALSE), c(FALSE, TRUE)))
})

test_that("passingSiteswap computes period", {
  expect_equal(s6@period, 6L)
  expect_equal(s7@period, 2L)
  expect_equal(sf@period, 3L)
})

test_that("passingSiteswap computes symmetry", {
  expect_equal(s6@symmetry, "asymmetrical") # period 6 → even
  expect_equal(s7@symmetry, "asymmetrical") # period 2 → even
  expect_equal(sf@symmetry, "symmetrical") # period 3 → odd
})

test_that("passingSiteswap computes n_props", {
  expect_equal(s6@n_props, 6) # 36/6
  expect_equal(s7@n_props, 7) # 14/2
  expect_equal(sf@n_props, 7) # 21/3
})

test_that("passingSiteswap valid is TRUE for valid patterns", {
  expect_true(s6@valid)
  expect_true(s7@valid)
  expect_true(sf@valid)
})

test_that("passingSiteswap valid is FALSE for collision", {
  expect_false(s_invalid@valid)
  expect_false(s_invalid@can_throw)
})

test_that("passingSiteswap fractional invalid pattern is FALSE", {
  # <4.5 3 | 3 4.5>: both A's pass and B's pass land at same juggler/beat
  expect_false(passingSiteswap("<4.5 3 | 3 4.5>")@valid)
})

# Compact == spaced ----------------------------------------------------------

test_that("compact and spaced p-notation are equivalent", {
  compact <- passingSiteswap("<3p33333|3p33333>")
  spaced <- passingSiteswap("<3p 3 3 3 3 3 | 3p 3 3 3 3 3>")
  expect_equal(compact@period, spaced@period)
  expect_equal(compact@n_props, spaced@n_props)
  expect_equal(compact@throws_by_juggler, spaced@throws_by_juggler)
  expect_equal(compact@is_pass_by_juggler, spaced@is_pass_by_juggler)
})

# Validation -----------------------------------------------------------------

test_that("passingSiteswap rejects invalid notation", {
  expect_error(passingSiteswap("531"), class = "jugglr_error_invalid_sequence")
  expect_error(
    passingSiteswap("<3p 3 3>"),
    class = "jugglr_error_invalid_sequence"
  )
  expect_error(passingSiteswap("<>"), class = "jugglr_error_invalid_sequence")
})

test_that("passingSiteswap rejects unequal juggler lengths", {
  expect_error(
    passingSiteswap("<3p 3 | 3p 3 3>"),
    class = "jugglr_error_invalid_sequence"
  )
})

test_that("passingSiteswap rejects fractional notation with 3+ jugglers", {
  expect_error(
    passingSiteswap("<4.5 3 | 3 4 | 3 3>"),
    class = "jugglr_error_invalid_sequence"
  )
})

# siteswap() dispatcher ------------------------------------------------------

test_that("siteswap() dispatches to passingSiteswap", {
  expect_s7_class(siteswap("<4p 3 | 3 4p>"), passingSiteswap)
})

# 3-juggler pattern ----------------------------------------------------------

test_that("3-juggler passingSiteswap has correct n_jugglers", {
  expect_equal(s3@n_jugglers, 3L)
})

test_that("3-juggler passingSiteswap is valid", {
  expect_true(s3@valid)
})

test_that("print shows collision reason for invalid passingSiteswap", {
  expect_message(print(s_invalid), "not a valid juggling pattern")
  expect_message(print(s_invalid), "collision")
})

test_that("print works for 3-juggler passingSiteswap", {
  expect_message(print(s3), "valid passing siteswap")
  expect_message(print(s3), "3 jugglers")
})

test_that("throw_data returns correct row count for 3-juggler pattern", {
  expect_equal(nrow(throw_data(s3, n_cycles = 2)), 3L * 3L * 2L)
})

# throw_data -----------------------------------------------------------------

test_that("throw_data returns a data frame for passingSiteswap", {
  expect_s3_class(throw_data(s7), "data.frame")
})

test_that("throw_data has expected columns for passingSiteswap", {
  expect_named(
    throw_data(s7),
    c(
      "beat",
      "juggler",
      "hand",
      "throw",
      "is_pass",
      "catch_beat",
      "catch_juggler",
      "catch_hand",
      "prop"
    )
  )
})

test_that("throw_data has n_jugglers * period * n_cycles rows", {
  expect_equal(nrow(throw_data(s7, n_cycles = 3)), 12L)
  expect_equal(nrow(throw_data(s6, n_cycles = 1)), 12L)
})

test_that("throw_data assigns a prop to every row", {
  expect_false(anyNA(throw_data(s7)$prop))
  expect_false(anyNA(throw_data(s6)$prop))
})

test_that("throw_data errors for invalid n_cycles", {
  expect_error(
    throw_data(s7, n_cycles = 0),
    class = "jugglr_error_bad_n_cycles"
  )
})

test_that("throw_data fractional: columns present and no NA props", {
  td <- throw_data(sf)
  expect_named(
    td,
    c(
      "beat",
      "juggler",
      "hand",
      "throw",
      "is_pass",
      "catch_beat",
      "catch_juggler",
      "catch_hand",
      "prop"
    )
  )
  expect_false(anyNA(td$prop))
})

# timeline -------------------------------------------------------------------

test_that("timeline returns a ggplot for passingSiteswap", {
  expect_s3_class(timeline(s7), "ggplot")
  expect_s3_class(timeline(s6), "ggplot")
  expect_s3_class(timeline(sf), "ggplot")
})

test_that("timeline works with title = FALSE", {
  p <- timeline(s7, title = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
})

test_that("timeline works with subtitle = FALSE", {
  p <- timeline(s7, subtitle = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$subtitle)
})

test_that("timeline works with n_cycles argument", {
  expect_s3_class(timeline(s7, n_cycles = 2), "ggplot")
})

# ladder ---------------------------------------------------------------------

test_that("ladder returns a ggplot for passingSiteswap", {
  expect_s3_class(ladder(s7), "ggplot")
  expect_s3_class(ladder(s6), "ggplot")
})

test_that("ladder works with vertical direction", {
  expect_s3_class(ladder(s7, direction = "vertical"), "ggplot")
})

test_that("ladder works with n_cycles argument", {
  expect_s3_class(ladder(s7, n_cycles = 2), "ggplot")
})

test_that("ladder works with hand_gap argument", {
  expect_s3_class(ladder(s7, hand_gap = 3L), "ggplot")
})

test_that("ladder respects title = FALSE", {
  p <- ladder(s7, title = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
})

test_that("ladder respects subtitle = FALSE", {
  p <- ladder(s7, subtitle = FALSE)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$subtitle)
})
