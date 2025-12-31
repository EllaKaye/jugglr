test_that("is_even works", {
  expect_true(is_even(2))
  expect_true(is_even(0))
  expect_false(is_even(1))
  expect_false(is_even(2.5))
})

test_that("is_whole_number works", {
  expect_true(is_whole_number(2))
  expect_true(is_whole_number(0))
  expect_true(is_whole_number(1))
  expect_true(is_whole_number(1L))
  expect_false(is_whole_number(2.5))
})

test_that("get_throws works", {
  expect_equal(get_throws("423"), c(4, 2, 3))
  expect_equal(get_throws("42a"), c(4, 2, 10))
  expect_equal(get_throws("42A"), c(4, 2, 10))
})
