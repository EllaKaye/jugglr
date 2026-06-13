test_that("is_sync_notation recognises correct notation", {
  expect_true(is_sync_notation("(4,4)"))
  expect_true(is_sync_notation("(4x,4x)"))
  expect_true(is_sync_notation("(ax,a)"))
  expect_true(is_sync_notation("(4,2x)(2x,4)"))
  expect_true(is_sync_notation("(4,2x)*"))
  expect_true(is_sync_notation("(4,1)")) # not valid, but allow odd here
})

test_that("is_sync_notation rejects incorrect notation", {
  expect_false(is_sync_notation("(4,4,4)"))
  expect_false(is_sync_notation("(4, 4)"))
  expect_false(is_sync_notation("(4 4)"))
  expect_false(is_sync_notation("(4,4y)"))
  expect_false(is_sync_notation("(4,2x)**"))
})

test_that("expand_siteswap works", {
  expect_equal(expand_siteswap("(4,2x)*"), "(4,2x)(2x,4)")
  expect_equal(expand_siteswap("(4,4)(4x,0)*"), "(4,4)(4x,0)(4,4)(0,4x)")
  expect_equal(expand_siteswap("(6x,4)(4,2x)*"), "(6x,4)(4,2x)(4,6x)(2x,4)")
})

test_that("slide works with valid sequences", {
  expect_equal(
    slide(c("4", "2x", "2x", "4")),
    list(slide1 = c(4, 1, 3, 4), slide2 = c(2, 3, 5, 2))
  )
  expect_equal(
    slide(c("6x", "4", "4", "2x", "4", "6x", "2x", "4")),
    list(
      slide1 = c(7, 4, 4, 1, 4, 5, 3, 4),
      slide2 = c(5, 6, 2, 3, 6, 3, 5, 2)
    )
  )
  expect_equal(
    slide(c("4", "6x", "2x", "4")),
    list(slide1 = c(4, 5, 3, 4), slide2 = c(6, 3, 5, 2))
  )
  expect_equal(
    slide(c("8x", "4x", "4", "4")),
    list(slide1 = c(9, 3, 4, 4), slide2 = c(5, 7, 4, 4))
  )
})

test_that("slide produces collisions for invalid sequences", {
  expect_false(can_throw(slide(c("6", "2", "4x", "6x"))$slide1))
  expect_false(can_throw(slide(c("6", "2", "4", "6"))$slide1))
})

test_that("slide works with letter throws", {
  expect_equal(
    slide(c("ax", "a")),
    list(slide1 = c(11, 10), slide2 = c(11, 10))
  )
})

# get_sync_throws ------------------------------------------------------------

test_that("get_sync_throws extracts throws from a sequence", {
  expect_equal(get_sync_throws("(4,2x)(2x,4)"), c("4", "2x", "2x", "4"))
  expect_equal(get_sync_throws("(4,4)"), c("4", "4"))
  expect_equal(get_sync_throws("(4x,4x)"), c("4x", "4x"))
})

# get_sync_hands -------------------------------------------------------------

test_that("get_sync_hands splits throws by hand", {
  expect_equal(
    get_sync_hands("(4,2x)(2x,4)"),
    list(hand_1 = c("4", "2x"), hand_2 = c("2x", "4"))
  )
  expect_equal(
    get_sync_hands("(4,4)"),
    list(hand_1 = "4", hand_2 = "4")
  )
})

# only_even_throws ------------------------------------------------------------

test_that("only_even_throws returns TRUE for even throws", {
  expect_true(only_even_throws("(4,4)"))
  expect_true(only_even_throws("(4,2x)(2x,4)"))
  expect_true(only_even_throws("(6x,4x)"))
})

test_that("only_even_throws returns FALSE for odd throws", {
  expect_false(only_even_throws("(4,1)"))
  expect_false(only_even_throws("(3,4)"))
})

# sync_symmetrical ------------------------------------------------------------

test_that("sync_symmetrical returns TRUE for symmetrical patterns", {
  expect_true(sync_symmetrical("(4,4)"))
  expect_true(sync_symmetrical("(4x,4x)"))
  expect_true(sync_symmetrical("(4,2x)(2x,4)"))
})

test_that("sync_symmetrical detects identity-rotation (self-mirror) patterns", {
  # Both hands throw identical heights, so the pattern is its own mirror image
  expect_true(sync_symmetrical("(4,4)(2,2)"))
  expect_true(sync_symmetrical("(4x,4x)(2,2)"))
})

test_that("sync_symmetrical returns FALSE for asymmetrical patterns", {
  expect_false(sync_symmetrical("(6x,4)(4,2x)"))
  expect_false(sync_symmetrical("(6x,4)"))
})
