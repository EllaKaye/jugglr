# Extracted from test-animate.R:157

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "jugglr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
path <- file.path(tempdir(), "test.gif")
local_mocked_bindings(
  download.file = function(...) invisible(0L),
  .package = "jugglr"
)
