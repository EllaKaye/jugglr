v <- vanillaSiteswap("531")
v_invalid <- vanillaSiteswap("432")
ps <- passingSiteswap("<4p 3 | 3 4p>")

# prop_color_scale -----------------------------------------------------------

test_that("prop_color_scale returns a ScaleDiscrete for <= 7 props", {
  s <- prop_color_scale(3L)
  expect_s3_class(s, "ScaleDiscrete")
})

test_that("prop_color_scale returns a ScaleDiscrete for > 7 props (fallback)", {
  s <- prop_color_scale(8L)
  expect_s3_class(s, "ScaleDiscrete")
})

# plot_subtitle --------------------------------------------------------------

test_that("plot_subtitle returns not-valid message for invalid siteswap", {
  expect_equal(plot_subtitle(v_invalid), "Not a valid juggling pattern.")
})

test_that("plot_subtitle contains type and n_props for valid siteswap", {
  sub <- plot_subtitle(v)
  expect_true(grepl("vanilla", sub))
  expect_true(grepl("3", sub))
})

test_that("plot_subtitle mentions n_jugglers for passingSiteswap", {
  sub <- plot_subtitle(ps)
  expect_true(grepl("jugglers", sub))
})

test_that("plot_subtitle appends extra note", {
  sub <- plot_subtitle(v, extra = "Test note.")
  expect_true(grepl("Test note", sub))
})

# title_subtitle_theme -------------------------------------------------------

test_that("title_subtitle_theme returns a theme object for NULL direction", {
  expect_s3_class(title_subtitle_theme(), "theme")
})

test_that("title_subtitle_theme returns a theme object for horizontal direction", {
  expect_s3_class(title_subtitle_theme("horizontal"), "theme")
})

test_that("title_subtitle_theme returns a theme object for vertical direction", {
  expect_s3_class(title_subtitle_theme("vertical"), "theme")
})
