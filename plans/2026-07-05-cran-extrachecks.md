# CRAN extra checks: prepare jugglr for first submission

## Context

The user ran `/r-lib:cran-extrachecks` to prepare jugglr for its **first CRAN submission** at version **0.1.0**. A systematic review against the CRAN extra-checks checklist found the package already largely compliant:

- `NEWS.md` and `cran-comments.md` exist; cran-comments already includes the `\dontrun{}` justification for `animate()` (external JugglingLab server) and a method-references note.
- DESCRIPTION Title is title case, 45 chars, no redundant phrases; `Authors@R` has `[cph]`; Description doesn't start with a forbidden phrase and quotes 'JugglingLab'.
- All 10 exported functions have `@returns` and `@examples`; only `animate()` uses `\dontrun{}` (justified). No commented-out example code.
- `doclisting` (Suggests, used in roxygen inline code) **is on CRAN** — no dependency problem.
- `urlchecker::url_check()` passes (all 12 URLs OK). Note: needs pandoc on PATH; use `PATH="/Applications/Positron.app/Contents/Resources/app/quarto/bin/tools/aarch64:$PATH"` when running it via `Rscript --quiet --vanilla`.
- GPL (>= 3) licence with `LICENSE.md` correctly `.Rbuildignore`d; no year to update.
- README images are relative paths into `man/figures/` which ship in the built package — fine.

Remaining gaps to fix (all confirmed with the user):

## Changes

### 1. DESCRIPTION — split Description into three sentences

File: `DESCRIPTION`. Replace the current two-sentence Description (the second sentence is very long) with three sentences, same content:

```
Description: Validate and visualise juggling patterns expressed in siteswap
    notation. Supports vanilla, synchronous, multiplex, synchronous multiplex,
    and passing siteswap, with tools to check pattern validity and to retrieve
    raw throw data for custom visualisations. Patterns can be plotted as
    timeline arc diagrams or ladder diagrams, and animated via the
    'JugglingLab' animation server
    (<https://jugglinglab.org/html/animinfo.html>).
```

This also corrects the server URL (per user: the current `<https://jugglinglab.org/>` should be `<https://jugglinglab.org/html/animinfo.html>`, matching the URL already used in `R/animate.R`).

### 2. .Rbuildignore — remove stale entries

File: `.Rbuildignore`. Delete the two lines `plot_experiments.R` and `test_class.R` (files no longer exist).

**Commit 1** after steps 1–2: "Prepare DESCRIPTION and .Rbuildignore for CRAN" (then run `cca`).

### 3. README — CRAN install instructions and status badge (DEFERRED — do not implement now)

Per the user: implement only steps 1 and 2 now; keep this section in the plan for a later session.

File: `README.Rmd` **only** (never edit README.md directly), then re-render.

- In the badges block (after line 19 `<!-- badges: start -->`), add:
  `[![CRAN status](https://www.r-pkg.org/badges/version/jugglr)](https://CRAN.R-project.org/package=jugglr)`
  (aspirational URL — leave as-is even though it 404s pre-acceptance; do NOT "fix" it if urlchecker flags it).
- In the Installation section, add a CRAN install block before the existing GitHub instructions:

  ```
  You can install jugglr from [CRAN](https://CRAN.R-project.org/package=jugglr) with:

  ``` r
  install.packages("jugglr")
  ```

  Or install the development version from [GitHub](https://github.com/EllaKaye/jugglr) with: ...
  ```

- Re-render with `Rscript --quiet --vanilla -e "devtools::build_readme()"` (commit both README.Rmd and the regenerated README.md).

**Commit 2** (when this deferred step is done): "Add CRAN install instructions and status badge to README" (then `cca`).

No NEWS.md bullet is needed for any of these changes (release prep / docs only).

## Verification (for steps 1–2 now)

1. `Rscript --quiet --vanilla -e "devtools::test(reporter = 'check')"` — all tests pass (nothing should be affected, but confirm).
2. Re-run URL check (with pandoc PATH prefix above) to confirm the new animinfo URL in DESCRIPTION resolves.
3. `R CMD build`-level sanity isn't needed for these two metadata edits; the user can fold `devtools::check()` into the later release workflow. When the README step is eventually done, visually confirm the rendered README.md shows both install blocks and the new badge.

## Out of scope / left as-is

- **Version bump to 0.1.0 and NEWS.md header** — deferred; the user will do this later once vignette work is finished.
- `cran-comments.md` — already correct for a first submission.
- `\dontrun{}` on `animate()` examples — justified and documented.
- The actual submission (`devtools::submit_cran()`) — user's call after the plan lands.
