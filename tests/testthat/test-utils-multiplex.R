# is_multiplex_notation --------------------------------------------------

test_that("is_multiplex_notation returns TRUE for valid multiplex", {
  expect_true(is_multiplex_notation("[43]1"))
  expect_true(is_multiplex_notation("[33]"))
  expect_true(is_multiplex_notation("24[54]"))
  expect_true(is_multiplex_notation("[32]1"))
  expect_true(is_multiplex_notation("[531]"))
})

test_that("is_multiplex_notation returns FALSE for vanilla siteswap", {
  expect_false(is_multiplex_notation("531"))
  expect_false(is_multiplex_notation("3"))
})

test_that("is_multiplex_notation returns FALSE for sync notation", {
  expect_false(is_multiplex_notation("(4,2x)*"))
  expect_false(is_multiplex_notation("(4,4)"))
})

test_that("is_multiplex_notation returns FALSE for invalid strings", {
  expect_false(is_multiplex_notation("[43"))
  expect_false(is_multiplex_notation("43]"))
  expect_false(is_multiplex_notation("[]"))
})

# get_multiplex_slots ----------------------------------------------------

test_that("get_multiplex_slots parses multiplex slots correctly", {
  expect_equal(get_multiplex_slots("[43]1"), list(c(4L, 3L), c(1L)))
  expect_equal(get_multiplex_slots("[33]"), list(c(3L, 3L)))
  expect_equal(get_multiplex_slots("24[54]"), list(c(2L), c(4L), c(5L, 4L)))
})

test_that("get_multiplex_slots handles letter throws", {
  expect_equal(get_multiplex_slots("[a4]"), list(c(10L, 4L)))
})

# can_multiplex_throw ----------------------------------------------------

test_that("can_multiplex_throw returns TRUE for valid patterns", {
  expect_true(can_multiplex_throw(list(c(4L, 3L), c(1L)))) # [43]1
  expect_true(can_multiplex_throw(list(c(3L, 3L)))) # [33]
  expect_true(can_multiplex_throw(list(c(3L, 2L), c(1L)))) # [32]1
  expect_true(can_multiplex_throw(list(c(5L, 4L), c(1L)))) # [54]1
})

test_that("can_multiplex_throw returns FALSE for invalid patterns", {
  # [53]4: n_props=6 but all 3 throws land at slot 2 while slot 1 gets nothing
  expect_false(can_multiplex_throw(list(c(5L, 3L), c(4L))))
})
