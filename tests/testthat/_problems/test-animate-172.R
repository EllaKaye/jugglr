# Extracted from test-animate.R:172

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "jugglr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
path <- file.path(tempdir(), "test.gif")
downloaded_url <- NULL
local_mocked_bindings(
  download.file = function(url, ...) {
    downloaded_url <<- url
    invisible(0L)
  },
  .package = "jugglr"
)
