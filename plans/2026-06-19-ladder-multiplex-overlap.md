# Separate overlapping ladder arcs for identical multiplex throws

> On implementation, save this plan to `plans/2026-06-19-ladder-multiplex-overlap.md`
> (per CLAUDE.md) and ensure `^plans$` is in `.Rbuildignore`.

## Context

A recent fix ([R/utils-plotting.R](R/utils-plotting.R) `fan_duplicate_heights()`,
commit `0127237`) solved overlapping arcs in **timeline** plots for identical
multiplex throws such as `siteswap("[33]")`: duplicate throws had their parabola
**peak heights** fanned symmetrically (±18%) so each prop's arc is visible.

The same overlap exists in **ladder** plots and is currently unhandled. For
`[33]`, `throw_data()` produces two rows with identical
`beat/hand/throw/catch_beat/catch_hand` but distinct `prop`. They render directly
on top of each other, so only the last-drawn prop colour is visible.

Ladder throws split into two render paths:
- **Even-height** (same-hand) throws → curved Bézier arcs via `create_curve_points()` (`geom_path`).
- **Odd-height** (cross-hand) throws → straight `geom_segment`. `[33]` (height 3, odd) is this case.

**Goal:** mirror the timeline fix in ladder plots so duplicate multiplex throws
are visibly separated, anchored at their true nodes, across all multiplex-capable
types (`multiplexSiteswap`, `synchronousMultiplexSiteswap`, and passing variants).

## Key insight (verified)

For a horizontal ladder, `create_curve_points()` uses control point
`(mean(x_start, x_end), 0.5)` for every throw. For an **odd cross-hand** throw
from `(beat, 0)` to `(catch_beat, 1)`, that control point is the exact segment
midpoint, so the quadratic Bézier reduces to a **straight line** (algebraically
verified: `x = beat + (Δbeat)·t`, `y = t`). For an **even same-hand** throw it
produces the existing bow toward centre.

So even and odd throws can share **one** curve generator, and a symmetric fan
applied to the control point's **hand-axis** coordinate (`y_control` horizontal /
`x_control` vertical) separates duplicates in both cases:
- non-duplicated throws (fan = 0) render exactly as today (straight stays straight);
- duplicates bow/peak symmetrically apart.

## Approach: fan the Bézier control point (mirrors timeline fix)

### 1. Shared fan-rank helper — [R/utils-plotting.R](R/utils-plotting.R)
Add a helper that assigns each throw a centred rank within its duplicate group:

```r
duplicate_fan_rank <- function(data, ...group_cols...) {
  data |>
    dplyr::group_by(<keys>) |>
    mutate(fan = dplyr::row_number() - (dplyr::n() + 1) / 2) |>
    dplyr::ungroup()
}
```

`fan` is `0` for unique throws, and e.g. `-0.5, +0.5` for a pair like `[33]`.
Optionally refactor `fan_duplicate_heights()` to reuse this rank (it currently
recomputes the same `row_number() - (n+1)/2` expression at line 468).

### 2. Extend `create_curve_points()` ([R/utils-plotting.R:124](R/utils-plotting.R#L124))
Add a `fan = 0` parameter and a small module-level `ladder_fan_spread` constant.
Offset the hand-axis control coordinate:
- horizontal: `y_control <- 0.5 + fan * ladder_fan_spread`
- vertical:   `x_control <- 0.5 + fan * ladder_fan_spread`

Keep the time-axis control coordinate as the beat midpoint. Tune
`ladder_fan_spread` (~0.3–0.5) so apex/bow separation is clearly visible while
control coords stay within the `hand_limits = c(-0.2, 1.2)` band.

### 3. `build_ladder_plot()` ([R/utils-plotting.R:150](R/utils-plotting.R#L150))
- Apply `duplicate_fan_rank()` grouped by `beat, hand, throw, catch_beat, catch_hand`.
- **Route all throws through `create_curve_points()`**, passing each throw's `fan`;
  drop the odd/`geom_segment` split (lines 181–184, 247–261). Non-duplicated odd
  throws still render as exact straight lines (proven above); even arcs keep their
  bow plus the fan. Preserve the per-throw `group` id (includes `prop`).
- Single `geom_path` over the combined curve data (lines 232–245 already do this).

### 4. `build_passing_ladder_plot()` ([R/utils-plotting.R:501](R/utils-plotting.R#L501))
Apply the same fan, grouping by `beat, hand, throw, catch_beat, catch_hand,
juggler, catch_juggler`. Offset the control point's cross-(hand)-axis coordinate
by `fan * ladder_fan_spread`:
- self-even arcs (lines 540–572): add the fan to the existing `y_ctrl`/`x_ctrl`
  (built around `y_center`).
- pass / odd straight throws (lines 537–538, 599–613): route duplicated ones
  through a Bézier with control = endpoint midpoint + fan offset so they bow
  apart; non-duplicated ones (fan = 0) stay straight. Factor the inline Bézier
  (lines 555–564) into the shared generator if practical.

### 5. Method wiring (no change expected)
`multiplexSiteswap`, `synchronousMultiplexSiteswap`, and passing `ladder` methods
all funnel through `build_simple_ladder()` / `build_passing_ladder_plot()`, so the
fix is centralised. The `is_even` column is already supplied by each method.

## Critical files
- [R/utils-plotting.R](R/utils-plotting.R) — `create_curve_points`, `build_ladder_plot`, `build_passing_ladder_plot`, new fan helper.
- [tests/testthat/test-multiplexSiteswap.R](tests/testthat/test-multiplexSiteswap.R) — ladder fan test alongside the existing timeline fan test (lines 131–142).
- `tests/testthat/test-synchronousMultiplexSiteswap.R` and `test-passingSiteswap.R` — sync-multiplex and passing ladder fan tests.
- `NEWS.md` — user-facing bullet.

## Tests
Mirror the timeline test (test-multiplexSiteswap.R:131). For `[33]` (odd) and an
even-duplicate case (e.g. `[44]`), build the ladder plot data and assert duplicate
throws receive **distinct** control points / fan values (`n_distinct(fan) == n`
per slot) so their rendered paths no longer coincide. Add an analogous test for a
sync-multiplex pattern and a passing-multiplex pattern. Keep tests minimal per
CLAUDE.md conventions.

## Verification
```
Rscript --quiet --vanilla -e 'devtools::load_all(); print(ladder(siteswap("[33]")))'   # odd: bows apart
Rscript --quiet --vanilla -e 'devtools::load_all(); print(ladder(siteswap("[44]")))'   # even: fanned apex
# plus a synchronous-multiplex and a passing-multiplex pattern, both directions (h/v)
Rscript --quiet --vanilla -e 'devtools::test(filter = "multiplexSiteswap", reporter = "check")'
Rscript --quiet --vanilla -e 'devtools::test()'   # full suite
air format .
```
Visually confirm: non-duplicated throws look identical to before (straight stays
straight); duplicate multiplex throws are clearly separated and still anchored at
the correct beat/hand nodes; colours for each prop are now both visible.

## Code comments
Match the existing "why" comment style (e.g. the explanatory block above
`fan_duplicate_heights()` at [R/utils-plotting.R:459](R/utils-plotting.R#L459)).
Where we deviate from the obvious, add a short comment explaining the *reason*:
- why even and odd throws are unified through one Bézier generator (a Bézier with
  control `(midpoint, 0.5)` draws an exact straight line for cross-hand throws);
- why the control point's hand-axis coordinate is fanned (to separate otherwise
  identical, fully overlapping multiplex arcs — the ladder analogue of the
  timeline peak-height fan).

## Commits
1. Helper + `create_curve_points` fan parameter.
2. `build_ladder_plot` unification + fan (with tests).
3. `build_passing_ladder_plot` fan (with tests).
4. NEWS.md bullet + docs (`devtools::document()` if signatures change).

Run `cca` after each commit.
