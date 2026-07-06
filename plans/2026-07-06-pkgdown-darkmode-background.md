# Ladder vs timeline dark-mode background discrepancy — investigation only, no change

## Conclusion

No code change. This was a diagnostic investigation, not an implementation task. Decision: leave `R/utils-plotting.R` as-is.

## What was asked

On the pkgdown site in dark mode, `timeline` plots blend into the page's dark grey background, but `ladder` plots show as pure black rectangles. Why the discrepancy?

## Root cause (confirmed)

Two things combine, and both are pkgdown-specific:

1. **pkgdown's dark-mode CSS inverts plot images.** pkgdown ships a built-in rule (in the generated `bootstrap.min.css`) that applies `filter: invert(100%) hue-rotate(180deg)` to every R-plot image (`img.r-plt`) when the site is in dark mode. This is a known, intentional pkgdown/bslib feature (there's a documented `no-invert-dark-mode` opt-out class) — it recolours the *whole* raster image (background and text/lines together), which is why it can flip an opaque white background to black while simultaneously flipping black title text to white, staying legible.

2. **pkgdown renders vignette figures with a transparent device canvas (`bg = NA`) by default; `devtools::build_readme()` and the Quarto site's own knit use the default opaque-white device canvas.** This is what actually causes the discrepancy to surface only on pkgdown:
   - `timeline()` uses `theme_void()`, whose `plot.background` is fully transparent — on pkgdown's transparent-canvas rendering, this stays truly transparent (alpha 0), so the page's own dark background shows through, and the invert filter has nothing opaque to act on there.
   - `ladder()` (`build_simple_ladder()`, `build_passing_ladder_plot()`) uses `theme_minimal()`, whose `plot.background` in ggplot2 ≥ 4.0 defaults to `element_rect(fill = "white")` — this bakes an opaque white panel rect into the PNG (confirmed via `png::readPNG()` pixel inspection: panel region is `(1,1,1,1)`). On pkgdown, the invert filter turns that opaque white into opaque black.
   - On GitHub (README) and the personal Quarto site, there is no such transparent-canvas rendering and no invert CSS — both plot types render with an opaque white background there today (confirmed by the user), so no discrepancy is visible in those contexts.

## Why "just make ladder transparent" was rejected

Transparent-background-with-black-text is only safe on pkgdown specifically, because pkgdown's invert filter recolours text and background together in lockstep. On GitHub, VS Code, RStudio's viewer, or other dark-themed contexts that don't apply that filter, a transparent background combined with baked-in black title/subtitle text would sit unmodified against a dark host background — risking illegible dark-on-dark text. This risk already exists today for `timeline()` (transparent + black text) outside pkgdown; changing `ladder()` to match would only extend, not fix, that exposure. The user confirmed both plot types currently show comfortably legible opaque-white backgrounds on GitHub and their personal site, and is not concerned about that.

## Decision and rationale

- Everything is legible everywhere today. The pkgdown dark-mode background inconsistency is cosmetically odd but not a functional problem.
- The user will handle their personal Quarto site's dark-mode rendering separately, controlling background and foreground colours together for that context specifically.
- The user is not concerned about the plain white-background appearance of plots on GitHub's dark mode.
- Given the cross-context legibility trade-offs (see above), leaving `R/utils-plotting.R` unchanged is the safest choice for now.

## Possible future option (not pursued)

If pkgdown dark-mode consistency is revisited later, a non-invasive option that doesn't touch the shared ggplot2 theme code (and so wouldn't affect the README or Quarto renders) would be to force pkgdown's own figure rendering to use an opaque device background instead of transparent, via `_pkgdown.yml`'s `figures:` config (e.g. `dev.args`/`bg` options). That would make `timeline`'s canvas opaque white too, matching `ladder`, GitHub, and the Quarto site — so pkgdown's invert CSS would then turn *both* plot types into a consistent dark card in dark mode, rather than making either one transparent.
