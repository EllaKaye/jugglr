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

# warn_if_props_hidden -------------------------------------------------------

test_that("warn_if_props_hidden warns when props are hidden in timeline", {
  s <- vanillaSiteswap("3") # 3 props, period 1; n_cycles=1 shows only 1 prop
  expect_warning(timeline(s, n_cycles = 1))
})

test_that("warn_if_props_hidden is silent for invalid siteswap", {
  s_bad <- vanillaSiteswap("432")
  expect_no_warning(timeline(s_bad, n_cycles = 1))
})

test_that("warn_if_props_hidden is silent when all props are shown", {
  s <- vanillaSiteswap("3") # n_cycles=3 shows all 3 props
  expect_no_warning(timeline(s, n_cycles = 3))
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

test_that("title_subtitle_theme spans title/subtitle across the full plot", {
  th <- title_subtitle_theme()
  expect_equal(th$plot.title.position, "plot")
  expect_equal(th$plot.caption.position, "plot")
})
