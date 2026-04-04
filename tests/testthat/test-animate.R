# format_color ---------------------------------------------------------------

test_that("format_color wraps RGB values in braces", {
  expect_equal(format_color(c(255, 0, 0)), "{255,0,0}")
  expect_equal(format_color(c(0, 128, 255)), "{0,128,255}")
})

# format_colors --------------------------------------------------------------

test_that("format_colors converts a single colour name to RGB string", {
  expect_equal(format_colors("red"), "{255,0,0}")
  expect_equal(format_colors("blue"), "{0,0,255}")
})

test_that("format_colors concatenates multiple colours", {
  expect_equal(format_colors(c("red", "blue")), "{255,0,0}{0,0,255}")
})

# colors_string --------------------------------------------------------------

test_that("colors_string returns 'colors=mixed' for 'mixed'", {
  expect_equal(colors_string("mixed"), "colors=mixed")
})

test_that("colors_string returns 'colors=orbits' for 'orbits'", {
  expect_equal(colors_string("orbits"), "colors=orbits")
})

test_that("colors_string formats named colours as RGB", {
  expect_equal(colors_string("red"), "colors={255,0,0}")
  expect_equal(colors_string(c("red", "blue")), "colors={255,0,0}{0,0,255}")
})

# fmt_string -----------------------------------------------------------------

test_that("fmt_string returns 'arg=value' string", {
  expect_equal(fmt_string("bps", 2), "bps=2")
  expect_equal(fmt_string("width", 400), "width=400")
})

test_that("fmt_string errors if value is not numeric", {
  expect_error(
    fmt_string("bps", "fast"),
    class = "jugglr_error_class_or_length"
  )
})

test_that("fmt_string errors if value is not length 1", {
  expect_error(
    fmt_string("bps", c(2, 3)),
    class = "jugglr_error_class_or_length"
  )
})

# jugglinglab_url ------------------------------------------------------------

test_that("jugglinglab_url builds a basic URL", {
  expect_equal(
    jugglinglab_url("3"),
    "https://jugglinglab.org/anim?pattern=3;redirect=true"
  )
})

test_that("jugglinglab_url always ends with redirect=true", {
  url <- jugglinglab_url("531")
  expect_true(endsWith(url, "redirect=true"))
})

test_that("jugglinglab_url includes colors=mixed and colors=orbits", {
  expect_true(grepl("colors=mixed", jugglinglab_url("3", colors = "mixed")))
  expect_true(grepl("colors=orbits", jugglinglab_url("3", colors = "orbits")))
})

test_that("jugglinglab_url includes RGB colours", {
  url <- jugglinglab_url("3", colors = "red")
  expect_true(grepl("colors=\\{255,0,0\\}", url))
})

test_that("jugglinglab_url includes named numeric params", {
  expect_true(grepl("bps=2", jugglinglab_url("3", bps = 2)))
  expect_true(grepl("width=400", jugglinglab_url("3", width = 400)))
  expect_true(grepl("height=300", jugglinglab_url("3", height = 300)))
  expect_true(grepl("fps=25", jugglinglab_url("3", fps = 25)))
  expect_true(grepl("slowdown=2", jugglinglab_url("3", slowdown = 2)))
})

test_that("jugglinglab_url includes valid dot args in URL", {
  url <- jugglinglab_url("3", dwell = 0.5)
  expect_true(grepl("dwell=0.5", url))

  url <- jugglinglab_url("3", border = 10)
  expect_true(grepl("border=10", url))
})

test_that("jugglinglab_url errors on invalid dot args", {
  expect_error(jugglinglab_url("3", foo = 1))
  expect_error(jugglinglab_url("3", foo = 1, bar = 2))
})

test_that("jugglinglab_url errors on non-scalar dot args", {
  expect_error(jugglinglab_url("3", dwell = c(0.5, 1)))
})

test_that("jugglinglab_url signals a message for ignored args", {
  expect_message(jugglinglab_url("3", startpaused = 1), "not supported")
  expect_message(jugglinglab_url("3", view = "JML"), "not supported")
})

test_that("jugglinglab_url does not include ignored args in URL", {
  url <- suppressMessages(jugglinglab_url("3", startpaused = 1))
  expect_false(grepl("startpaused", url))
})

# validate_path --------------------------------------------------------------

test_that("validate_path returns NULL invisibly for NULL input", {
  expect_null(validate_path(NULL))
})

test_that("validate_path errors if save is not a single string", {
  expect_error(validate_path(123))
  expect_error(validate_path(c("a.gif", "b.gif")))
})

test_that("validate_path errors for wrong file extension", {
  expect_error(validate_path(file.path(tempdir(), "out.mp4"), ext = "gif"))
})

test_that("validate_path errors if directory does not exist", {
  expect_error(validate_path("/nonexistent/path/out.gif"))
})

test_that("validate_path returns path invisibly for valid input", {
  path <- file.path(tempdir(), "out.gif")
  expect_equal(validate_path(path), path)
})

# animate --------------------------------------------------------------------

test_that("animate errors for wrong file extension", {
  expect_error(animate("3", path = file.path(tempdir(), "out.mp4")))
})

test_that("animate errors for nonexistent directory", {
  expect_error(animate("3", path = "/nonexistent/path/out.gif"))
})

test_that("animate errors for invalid dot args", {
  expect_error(animate("3", foo = 1))
})

test_that("animate with path downloads and returns path invisibly", {
  path <- file.path(tempdir(), "test.gif")
  local_mocked_bindings(
    download.file = function(...) invisible(0L),
    .package = "utils"
  )
  result <- withVisible(animate("3", path = path))
  expect_false(result$visible)
  expect_equal(result$value, path)
})

test_that("animate with path calls download.file with the gif URL", {
  path <- file.path(tempdir(), "test.gif")
  downloaded_url <- NULL
  local_mocked_bindings(
    download.file = function(url, ...) {
      downloaded_url <<- url
      invisible(0L)
    },
    .package = "utils"
  )
  animate("3", path = path)
  expect_true(startsWith(downloaded_url, "https://jugglinglab.org/anim?"))
  expect_true(grepl("pattern=3", downloaded_url))
})

test_that("animate without path writes HTML and returns temp file invisibly", {
  withr::local_options(viewer = function(url) invisible(NULL))
  result <- withVisible(animate("3"))
  expect_false(result$visible)
  expect_true(endsWith(result$value, "juggling_animation.html"))
  expect_true(file.exists(result$value))
})

test_that("animate without path embeds the gif URL in the HTML", {
  withr::local_options(viewer = function(url) invisible(NULL))
  temp_file <- animate("3")
  html <- paste(readLines(temp_file), collapse = "\n")
  expect_true(grepl("jugglinglab.org", html))
  expect_true(grepl("pattern=3", html))
})
