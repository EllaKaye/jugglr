# Fix marquee subtitle/panel overlap (shared plotting machinery)

## Context

Deferred from the 2026-06-19 `timeline()` overlap fix
(see [project_timeline_followups memory] / [plans/2026-06-19-timeline-overlap.md](plans/2026-06-19-timeline-overlap.md)).
Two follow-ups were on the table; **only task 1 is in scope**.

- **Task 1 (this plan) — marquee subtitle overlap bug.** The plot subtitle overlaps
  the panel when the panel is narrow. It reproduces **today in `ladder()`** (which
  uses `coord_fixed`), so it is a standalone, pre-existing bug.
- **Task 2 — timeline arc-density consistency — decided against.** It required
  `coord_fixed` + `beat_step` compression, but (a) `beat_step` compression is a no-op
  without `coord_fixed`, (b) the density "inconsistency" is purely a *cross-plot*
  comparison artifact — a single timeline reads identically with or without it, and
  (c) `coord_fixed` letterboxes short/period-1 patterns and forces a one-size-fits-all
  ratio. The current free-aspect rendering looks better per-plot, so it stays.

Intended outcome: titles/subtitles never collide with the panel, across all plot
types (ladder, passing ladder, timeline).

## Diagnosis — confirmed

The subtitle (`marquee::element_marquee(..., width = unit(1, "npc"))`, built in
`title_subtitle_theme()` / `plot_subtitle()`, [R/utils-plotting.R](R/utils-plotting.R))
is laid out in ggplot2's **panel column** — `g$layout` shows the subtitle spanning
cols `l=7, r=7`, identical to the panel. Its `width = 1 npc` therefore resolves
against the *panel* width, which `coord_fixed` can make arbitrarily narrow, so the
text wraps too narrowly and its row height is mis-measured, spilling into the panel.

Setting `plot.title.position = "plot"` re-spans the title/subtitle to the **full plot
width** (verified: span goes `l=7,r=7` → `l=3,r=11`). With the subtitle keyed to the
full plot width instead of the letterboxed panel, `1 npc` is stable and the overlap
resolves.

## Implementation

Single shared fix. In `title_subtitle_theme()`
([R/utils-plotting.R](R/utils-plotting.R), ~line 49), add to the returned `theme()`:

```r
plot.title.position = "plot",
plot.caption.position = "plot"
```

`title_subtitle_theme()` is called by `build_ladder_plot()`,
`build_passing_ladder_plot()`, and `build_simple_timeline()`, so ladders, passing
ladders, and timelines all benefit from the one change. Keep `width = unit(1, "npc")`
on the marquee element — it now resolves against the full plot width. Run `air format .`.

## Tests

- In the nearest existing plotting test file (e.g. `tests/testthat/test-ladder.R` or
  a `test-utils-plotting.R`), add a light check that a built plot's theme has
  `plot.title.position == "plot"` (`ggplot2::ggplot_build`/`p$theme` or
  `ggplot2::calc_element("plot.title.position", ...)`), locking the fix in. Pixel-level
  non-overlap is impractical to assert headlessly.
- Run the full suite: `Rscript -e "devtools::test(reporter = 'check')"` — all green
  before committing.

## Docs / NEWS

- Package is pre-first-CRAN-release and `ladder()`/`timeline()` subtitle layout is
  unreleased behaviour, so **no `NEWS.md` bullet** required. Flag to the user if they
  want one anyway.
- No vignette/README figures depend on subtitle layout; no doc regeneration needed.

## Verification (end-to-end)

```r
Rscript --quiet --vanilla -e 'devtools::load_all()
  ggplot2::ggsave("/tmp/ladder-531.png",  ladder(siteswap("531")))
  ggplot2::ggsave("/tmp/ladder-44.png",   ladder(siteswap("(4,4)")))   # narrow panel
  ggplot2::ggsave("/tmp/timeline-531.png", timeline(siteswap("531")))'
```
Render at a couple of device aspect ratios and confirm the multi-line subtitle no
longer overlaps the panel in any of them.

## Memory

After implementing, update `project_timeline_followups.md`: mark task 1 **done** and
record that task 2 (arc-density consistency) was **considered and decided against**
(reasons above), so it isn't picked up again as open work.

## Commits (run `cca` after each)

1. `plot.title.position = "plot"` in `title_subtitle_theme()` + theme test.
2. **Chore:** copy this plan to `plans/2026-06-19-subtitle-overlap-fix.md`
   (`^plans$` already in `.Rbuildignore`).
```
