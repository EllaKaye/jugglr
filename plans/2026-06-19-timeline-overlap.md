# Timeline overlapping-arc fix (two-sided + fan)

## Context

`timeline()` draws each throw as a parabolic arc (height = throw value, x-span =
beat → catch beat), coloured by prop, built in `build_simple_timeline()`
([R/utils-plotting.R](R/utils-plotting.R)). It broke whenever two arcs were
**identical**, because the second drew on top of the first:

- **Synchronous** `(4,4)` — both hands throw a straight 4 at the same beat, so the
  parabolas coincide. The old solid/dashed encoding collapsed to a single dashed
  arc and the subtitle ("Solid and dashed lines represent different hands") was
  misleading. `(4,4)(4x,4x)` has the same overlap plus hand-changing throws.
- **Multiplex** `[33]` — one hand throws two *equal* 3s per beat; the arcs coincide,
  so only 3 of 6 props were visible (`[53]` is fine — only equal heights collide).

Intended outcome: every prop's trajectory is visually distinct and the two hands of
a synchronous pattern are unambiguous without a legend.

A *secondary* concern surfaced during review — `(4,4)` arcs look ~2× more spaced
than `(4,4)(4x,4x)` — but that needs a fixed aspect ratio (`coord_fixed`), which
exposes a pre-existing marquee title/subtitle overlap bug (present in `ladder()`
too). **Per decision, that density work and the subtitle bug are handled
separately** and are out of scope here (see Deferred follow-up).

## Implementation (core fix — already written, needs finalising)

All changes are in `build_simple_timeline()` and the two synchronous timeline
methods. `throw_data()` already carries `hand`, `is_crossing`, `prop`, `beat`,
`catch_beat`, `throw`.

1. **Two-sided synchronous rendering** — in `build_simple_timeline()` add a
   `two_sided` flag. When set, negate `y` for hand-1 rows so hand 0 arcs sit above
   a faint `geom_hline(yintercept = 0)` and hand 1 mirrors below. Drop the
   `linetype` aesthetic entirely; mapping is always `aes(x, y, group =
   interaction(beat, prop), color = prop)`.
2. **Per-hand axis labels** — `sync_hand_labels(full_sequence)` splits each
   `"(a,b)"` slot into top-hand (`a`) and bottom-hand (`b`) labels. Render the
   bottom labels on the primary x-axis and the top labels via
   `sec.axis = dup_axis(...)`, with both `axis.text.x.top`/`.bottom` bold.
3. **Nested-height fan** — `fan_duplicate_heights()` gives each set of identical
   arcs (same `beat`/`hand`/`throw`/`catch_beat`) a slightly different `peak`
   height (symmetric fan, `fan_offset ≈ 0.18`), so duplicate multiplex throws
   render as concentric parabolas. Non-duplicates keep their exact height. Applies
   automatically to both `multiplexSiteswap` and `synchronousMultiplexSiteswap`.
4. **Methods** — `synchronousSiteswap` ([R/synchronousSiteswap.R](R/synchronousSiteswap.R))
   and `synchronousMultiplexSiteswap` ([R/synchronousMultiplexSiteswap.R](R/synchronousMultiplexSiteswap.R))
   pass `two_sided = TRUE`, `top_labels`, and
   `subtitle_extra = "Each hand's throws are shown above and below the centre line."`
   `vanillaSiteswap` and `multiplexSiteswap` are unchanged (single-sided).

### Revert the coord_fixed experiment
Roll back the density experiment so this PR is purely the overlap fix:
- Remove `coord_fixed(ratio = 0.5)` from `build_simple_timeline()`.
- Remove the `beat_step` parameter, the `df$x <- df$x / beat_step` and
  `df$y <- df$y / beat_step` scaling, and the `/ beat_step` on `beats`.
- Remove `beat_step = 2L` from both synchronous timeline methods.

The resulting two-sided plots fill the width as before (the version that rendered
cleanly), just with hands separated above/below.

## Tests (already updated)
- `test-synchronousSiteswap.R` / `test-synchronousMultiplexSiteswap.R`: replaced the
  obsolete `linetype`-aesthetic assertions with a check that the built arc data
  spans both negative and positive `y` (hands on opposite sides).
- `test-multiplexSiteswap.R`: added a check that `fan_duplicate_heights()` gives
  `[33]` two distinct peak heights per slot, so all 6 props are separable.
- Run the full suite with `devtools::test(reporter = "check")` and confirm green.

## Docs
- No `NEWS.md` bullet: `timeline()` is unreleased (development version / first
  release), so this refines an unreleased feature rather than changing released
  behaviour. (Flag for the user if they'd prefer one anyway.)
- Vignette/README need no changes: no synchronous timeline figures appear there and
  nothing references the old solid/dashed convention.
- Run `air format .`.

## Deferred follow-up (separate task — NOT this plan)
1. **Marquee title/subtitle overlap** bug (affects `ladder()` today and would block a
   fixed-aspect timeline). Fix the shared `title_subtitle_theme()` /
   `marquee::element_marquee()` layout in [R/utils-plotting.R](R/utils-plotting.R).
2. **Timeline arc-density consistency** via `coord_fixed` (+ sync beat compression so
   `(4,4)` reads at vanilla tempo), built on top of the subtitle fix.

## Verification
```r
Rscript --quiet --vanilla -e 'devtools::load_all();
  ggplot2::ggsave("/tmp/t44.png",  timeline(siteswap("(4,4)")));
  ggplot2::ggsave("/tmp/t44x.png", timeline(siteswap("(4,4)(4x,4x)")));
  ggplot2::ggsave("/tmp/t33.png",  timeline(siteswap("[33]")))'
```
Confirm: `(4,4)` shows two mirrored arcs (one up, one down), not a lone dashed line;
`(4,4)(4x,4x)` shows each prop's colour alternating sides across the cross beats;
`[33]` shows two distinct fanned arcs per slot (six over three slots).

## Commits
1. Two-sided rendering + per-hand labels + nested-height fan + updated tests
   (single commit — the rendering changes are interleaved in `build_simple_timeline()`).
2. Chore: copy this plan to `plans/2026-06-19-timeline-overlap.md`
   (`^plans$` is already in `.Rbuildignore`).

Run `cca` after each commit.
