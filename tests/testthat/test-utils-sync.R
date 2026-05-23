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
})

test_that("slide doesn't work with invalid sequences", {})
