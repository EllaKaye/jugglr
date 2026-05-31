test_that("siteswap dispatches to vanillaSiteswap for alphanumeric input", {
  expect_s7_class(siteswap("531"), vanillaSiteswap)
  expect_s7_class(siteswap("3"), vanillaSiteswap)
})

test_that("siteswap dispatches to synchronousSiteswap for sync input", {
  expect_s7_class(siteswap("(4,4)"), synchronousSiteswap)
  expect_s7_class(siteswap("(4,2x)*"), synchronousSiteswap)
})

test_that("siteswap dispatches to multiplexSiteswap for multiplex input", {
  expect_s7_class(siteswap("[43]1"), multiplexSiteswap)
  expect_s7_class(siteswap("[33]"), multiplexSiteswap)
})

test_that("siteswap errors for unrecognised notation", {
  expect_error(siteswap("not-valid"), class = "jugglr_error_not_valid_siteswap")
  expect_error(siteswap("5-3-1"), class = "jugglr_error_not_valid_siteswap")
})

test_that("siteswap errors for sync notation with odd throws", {
  expect_error(siteswap("(4,1)"), class = "jugglr_error_invalid_sequence")
})
