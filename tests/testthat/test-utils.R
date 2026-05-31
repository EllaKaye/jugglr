test_that("is_even works", {
  expect_true(is_even(2))
  expect_true(is_even(0))
  expect_false(is_even(1))
  expect_false(is_even(2.5))
})

test_that("is_odd works", {
  expect_true(is_odd(1))
  expect_false(is_odd(0))
  expect_true(is_odd(3))
  expect_false(is_odd(1.3))
})

test_that("is_whole_number works", {
  expect_true(is_whole_number(2))
  expect_true(is_whole_number(0))
  expect_true(is_whole_number(1))
  expect_true(is_whole_number(1L))
  expect_false(is_whole_number(2.5))
})

test_that("is_whole_number errors for non-numeric", {
  expect_error(is_whole_number("a"), class = "jugglr_error_not_numeric")
  expect_error(is_whole_number(c(1, 2)), class = "jugglr_error_not_numeric")
})

test_that("chr_sync_throws_to_num strips x suffix before converting", {
  expect_equal(chr_sync_throws_to_num(c("4x", "2x", "4")), c(4, 2, 4))
  expect_equal(chr_sync_throws_to_num(c("4", "2")), c(4, 2))
  expect_equal(chr_sync_throws_to_num(c("ax")), c(10))
})

test_that("chr_throws_to_num works", {
  expect_equal(chr_throws_to_num(c("4", "2", "3")), c(4, 2, 3))
  expect_equal(chr_throws_to_num(c("4", "2", "a")), c(4, 2, 10))
  expect_equal(chr_throws_to_num(c("4", "2", "A")), c(4, 2, 10))
  expect_equal(chr_throws_to_num(c("5", "5", "5", "5", "0")), c(5, 5, 5, 5, 0))
})

test_that("get_throws works", {
  expect_equal(get_throws("423"), c(4, 2, 3))
  expect_equal(get_throws("42a"), c(4, 2, 10))
  expect_equal(get_throws("42A"), c(4, 2, 10))
  expect_equal(get_throws("55550"), c(5, 5, 5, 5, 0))
})

test_that("get_throws errors for non-string input", {
  expect_error(get_throws(423), class = "jugglr_error_not_string")
  expect_error(get_throws(c("4", "2", "3")), class = "jugglr_error_not_string")
})

test_that("can_throw works", {
  expect_true(can_throw(c(4, 2, 3)))
  expect_false(can_throw(c(4, 3, 2)))
})
