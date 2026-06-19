# Refine ladder overlap fix: segments by default, curve only on overlap

> On implementation, update `plans/2026-06-19-ladder-multiplex-overlap.md`
> (per CLAUDE.md) to reflect this refinement.

## Context

The overlapping-arc fix for ladder plots is already implemented and committed
(`2e564ec`, `4d121f0`): identical multiplex throws (e.g. the two 3s in `[33]`)
are fanned apart by nudging a quadratic-Bézier control point. To do this the
implementation routed **every** ladder throw through `create_curve_points()` and
a single `geom_path`, replacing the original even-arc/odd-segment split.

On review, that unification is heavier than the problem needs: every straight
(odd, cross-hand) throw — the common case — is now a 50-point degenerate Bézier
instead of a cheap 2-point `geom_segment`, and the original deliberate
even=curve / odd=segment design was erased. It also forced a refactor of the
**passing** builder that turned out to be a no-op: `passingSiteswap` does not
expand multiplex notation (`<[33]|[33]>` parses to single throws), so passing has
no duplicate throws, the fan is always 0, and the rewritten passing builder
produces output identical to the original.

**Goal:** keep the (good) fan behaviour, but render straight throws with their
natural primitive again, and limit curve generation to throws that actually need
it: same-hand arcs plus the genuinely-overlapping duplicates. Revert the passing
builder to its original tested form.

## Design decisions (settled with the user)

- **`[333]` middle line stays straight.** The fan rank is symmetric and centred
  (`row_number() - (n+1)/2`), so a triple gets `-1, 0, +1`; the middle throw's
  `fan = 0` yields an exact straight line while the outer two bow apart. This is
  preserved because the *whole* duplicate group is routed through the curve
  generator (where `fan = 0` is exactly straight).
- **Partition on duplicate-group size, not `fan != 0`.** The middle of `[333]`
  has `fan = 0` yet is part of an overlapping group, so it must still be grouped
  with its siblings. Partition on `n > 1`.
- **Passing: full revert, no dormant fan code.** A passing pattern with two
  simultaneous identical throws cannot be constructed today, so any fan code in
  the passing builder would be untestable dead code. If multiplex/synchronous
  passing is added later, re-apply the simple builder's pattern then.

## Changes — all in [R/utils-plotting.R](R/utils-plotting.R)

### 1. `duplicate_fan_rank()` — also return group size
Add a group-size column so callers can partition on it:
```r
mutate(fan = dplyr::row_number() - (dplyr::n() + 1) / 2, n_dup = dplyr::n())
```
Harmless for the timeline caller (`fan_duplicate_heights()` ignores `n_dup`).

### 2. `build_ladder_plot()` — restore the segment/curve split
After `duplicate_fan_rank()` (grouped by `beat, hand, throw, catch_beat,
catch_hand`):
- **curved** = `is_even | n_dup > 1` → `purrr::pmap()` through
  `create_curve_points(..., fan = fan)` → one `geom_path` (as now).
- **straight** = `!is_even & n_dup == 1` → `geom_segment` (restore the original
  block: `x_start/y_start → x_end/y_end`, coloured by `factor(prop)`).

Even arcs keep their bow + fan; non-overlapping odd throws are cheap segments;
overlapping odd duplicates flow through the curve path and bow apart (middle of a
triple stays straight via `fan = 0`).

### 3. `create_curve_points()` — unchanged
Keeps its `fan` parameter, `same_hand` detection, and the two spreads
(`ladder_fan_spread_odd = 0.3`, `ladder_fan_spread_even = 0.14`). The curved set
contains both even arcs and bowed odd duplicates, so this logic is still needed.

### 4. `build_passing_ladder_plot()` — revert to original
Restore the original split: self-even throws → inline Bézier arc (`geom_path`,
control at `y_center`, no fan); passes and odd self-throws → straight
`geom_segment`. Remove the `duplicate_fan_rank()` call and all `fan`/`spread`
handling from this function.

## Tests — [tests/testthat/](tests/testthat/)
- Existing fan-rank tests stay valid (they call `duplicate_fan_rank()` directly):
  `test-multiplexSiteswap.R` (`[33]`) and `test-synchronousMultiplexSiteswap.R`
  (`([66],4)`).
- Add one targeted assertion that the segment/curve split works: e.g. for a
  plain pattern with odd non-duplicated throws (`siteswap("531")`), the built
  ladder has a `GeomSegment` layer carrying those throws; for `[33]` the
  overlapping odd duplicates instead appear in the `GeomPath` layer (no straight
  segments). Use `ggplot2::ggplot_build()` / layer classes. Keep it minimal.
- Passing smoke tests are unaffected by the revert (original code passed them).

## Verification
```
Rscript --quiet --vanilla -e 'devtools::load_all(); print(ladder(siteswap("[33]")))'    # odd dup: bows apart
Rscript --quiet --vanilla -e 'devtools::load_all(); print(ladder(siteswap("531")))'      # odd: plain segments
Rscript --quiet --vanilla -e 'devtools::load_all(); print(ladder(siteswap("[44]")))'     # even dup: fanned apex
Rscript --quiet --vanilla -e 'devtools::load_all(); print(ladder(siteswap("([66],4)")))' # sync-multiplex dup
Rscript --quiet --vanilla -e 'devtools::load_all(); print(ladder(passingSiteswap("<4 3 | 3 4>")))' # passing unchanged
Rscript --quiet --vanilla -e 'devtools::test()'   # full suite
air format .
```
Visually confirm: ordinary straight throws render as plain segments (as before
the fix); duplicate multiplex throws still separate; the middle of an odd triple
is straight; passing ladders are pixel-identical to before the original fix.

## Commits
1. `build_ladder_plot` segment/curve split + `duplicate_fan_rank` group size (with test).
2. Revert `build_passing_ladder_plot` to its original segment/curve form.

Run `cca` after each commit.
