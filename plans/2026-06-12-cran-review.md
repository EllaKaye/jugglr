# jugglr package review — pre-CRAN improvements

> On execution, copy this plan to `plans/2026-06-12-cran-review.md` in the project (per global CLAUDE.md convention; `^plans$` is not yet in `.Rbuildignore`, add it).

## Context

Ella asked for a review of jugglr ahead of a first CRAN submission. Overall verdict: the package is in genuinely good shape — `devtools::check()` is clean (0 errors / 0 warnings / 0 notes, vignettes skipped locally only because pandoc isn't on the CLI path), all 429 tests pass, error messages have classes throughout, S7 usage is idiomatic, and documentation is thorough. The review found **two real bugs** (both verified by running code), one wrong documented limitation, a mid-rewrite vignette with placeholder links, and a set of CRAN-prep gaps.

## Verified findings

### Bug 1: `sync_symmetrical()` misses the identity rotation
`R/utils-sync.R:85-87` builds rotations with `c(right[(k+1):n], right[seq_len(k)])`. When `k = n`, `(k+1):n` is the *descending* sequence `c(n+1, n)`, producing a length-`n+2` vector with an `NA` — so the un-rotated comparison (`left == right`) is never tested. Verified: `synchronousSiteswap("(4,4)(2,2)")@symmetry` returns `"asymmetrical"` but the pattern is its own mirror image (each hand throws the same heights), so it should be `"symmetrical"`. The sibling function `sync_multiplex_symmetrical()` (`R/utils-sync-multiplex.R:91-94`) already does this correctly with modular indexing: `right[((seq_len(n) - 1L + k) %% n) + 1L]`.

### Bug 2: `animate()` builds unencoded URLs — breaks passing patterns
`jugglinglab_url()` (`R/animate.R:313-316`) pastes the pattern into the URL raw. Verified: `animate("<3p 3 3 3 3 3 | 3p 3 3 3 3 3>", path = ...)` fails with "URL using bad/illegal format" because of the literal spaces, `<`, `|`, `>`. Verified that with `utils::URLencode()` the JugglingLab server renders a correct 323KB GIF for p-notation passing patterns — so the documented limitation in `animate.Rd`, the README, and the animate vignette ("passingSiteswap sequences are not valid JugglingLab notation") is wrong: **encoding the URL fixes the bug and unlocks passing-pattern animation**. Also verified: *fractional* notation (`"<4.5 3 3 | 3 4 3.5>"`) genuinely fails server-side (returns an "Oops! Juggling Lab rendering error" image), so that caveat stays.

### Bug 3 (minor): `Siteswap` base class is instantiable but broken
README calls `Siteswap` "an abstract parent class", but `jugglr:::Siteswap("531")` constructs fine and then errors confusingly at print time (`Can't find property <jugglr::Siteswap>@valid`).

### Bug 4 (minor): `siteswap()` with a character vector gives a raw base error
`siteswap(c("531", "441"))` → `the condition has length > 1` from `str_detect` inside `if` (`R/siteswap.R:77`), instead of a friendly classed cli error. (The class constructors themselves are fine — the S7 property validator catches it.)

### The Get Started vignette is mid-rewrite
`vignettes/jugglr.Rmd` has placeholder links — `[types of siteswap](LINK)` (line 25), `[S7](URL)` (line 27), `[whether it is valid](URL)` (line 29) — and typos: "distinguised", "sychronous" (line 25), "converys", "caught it" (line 33), "the same this" (line 30), "For the example" (line 21). (Matches the WIP commit "Start rewriting the jugglr vignette".)

### CRAN-prep gaps
- **Package size**: `vignettes/figures/` holds 2.3MB of GIFs that ship in the tarball and installed `doc/` — will trigger an installed-size NOTE. Resolution depends on Ella's choice (see question asked / answer below).
- **Version / NEWS**: version is `0.0.0.9000`; needs a bump (suggest `0.1.0`). `NEWS.md` reads as a dev changelog (bullets about doc tweaks and internal property-declaration fixes, which the project's own CLAUDE.md says to exclude; one bullet uses roxygen `[vanillaSiteswap]` link syntax that won't render). For a first release, condense to a short feature summary.
- **No `cran-comments.md`** — create with `usethis::use_cran_comments()`; note the justified `\dontrun{}` in `animate()` examples (network + opens viewer) and that there are no method references.
- **DESCRIPTION**: quote 'JugglingLab' in `Description:` and add the URL `<https://jugglinglab.org/>` after it; consider `Language: en-GB` (package uses UK spellings — "visualise" — which CRAN's spell check flags otherwise).
- **README**: installation section only covers GitHub; add the conventional CRAN block (`install.packages("jugglr")`) ahead of submission. Edit `README.Rmd` only, then `devtools::build_readme()`.
- Run `urlchecker::url_check()` before submission.

### Smaller code/documentation issues
1. User-facing error in `jugglinglab_url()` names the internal helper: "invalid argument(s) passed to `jugglinglab_url()`" (`R/animate.R:276`) — user called `animate()`; reword (and pass `call = rlang::caller_env()` as `fmt_string()` already does).
2. `throw_data()` and `ladder()` generic docs (`R/generics.R:8,73`) list only three of the five classes; `throw_data()` `@returns` omits the extra columns the passing method adds (`juggler`, `is_pass`, `catch_juggler`).
3. `hand_gap` argument of the passing `ladder()` method (`R/passingSiteswap.R:436`) is undocumented in `ladder.Rd`.
4. `animate()` doc says `prop` defaults to `"ball"` but the signature default is `NULL` (server default); align the wording.
5. The direction-normalisation block (`arg_match` + h/v collapse) is copy-pasted in all five `ladder()` methods — extract a `normalise_direction()` helper in `R/utils-plotting.R`.
6. The four near-identical `print` methods (base `Siteswap`, sync, sync-multiplex, passing) could share one helper taking an optional "extra bullets" argument — optional, low priority.
7. `validate_path()` error messages use backticked `` `path` `` instead of `{.arg path}` cli style used elsewhere.

## Implementation steps (with commit points)

1. **Fix `sync_symmetrical()`** (`R/utils-sync.R`): use the modular-rotation indexing from `sync_multiplex_symmetrical()` — ideally extract a shared `rotations_match()` helper used by both. Add tests: `sync_symmetrical("(4,4)(2,2)")` is `TRUE`, `synchronousSiteswap("(4x,4x)(2,2)")@symmetry == "symmetrical"`, plus the multiplex analogue. NEWS bullet. **Commit + `cca`.**
2. **Fix URL encoding in `jugglinglab_url()`** (`R/animate.R`): percent-encode each `key=value` segment value (`utils::URLencode(value, reserved = TRUE)` on values, not the whole URL, so `;` separators and `=` survive). Add tests for a passing pattern URL (no spaces/`<`/`|` in output; mock-free, just string checks). Update `animate()` docs, README.Rmd (+ rebuild README.md), and `vignettes/animate.Rmd`: p-notation passing patterns *can* be animated; fractional notation cannot (server rendering error). NEWS bullet. **Commit + `cca`.**
3. **Make `Siteswap` abstract + validate `siteswap()` input** (`R/siteswap.R`): `abstract = TRUE` on the base class; add a single-string check at the top of `siteswap()` with a classed cli error. Tests for both. NEWS bullet for `siteswap()` input validation. **Commit + `cca`.**
4. **Small code/doc fixes** (items 1–5, 7 above; skip 6 unless trivial): reworded error, doc corrections, `normalise_direction()` helper, `hand_gap` documented via the method or generic `@param`. `devtools::document()`. **Commit + `cca`.**
5. **Finish `vignettes/jugglr.Rmd`**: fill the three placeholder links (anchor link for types section; https://rconsortium.github.io/S7/ for S7; anchor or removal for "whether it is valid"), fix the six typos, proofread the whole file. **Commit + `cca`.**
6. **Handle the animate vignette size** per Ella's chosen option (pkgdown article via `usethis::use_article("animate")` — moves GIFs out of the tarball, keeps them on the site; update `vignette("animate")` cross-references in README, jugglr.Rmd, NEWS to article links). **Commit + `cca`.**
7. **CRAN admin**: bump Version to 0.1.0; condense NEWS.md under a `# jugglr 0.1.0` heading; `usethis::use_cran_comments()`; DESCRIPTION tweaks ('JugglingLab' + URL, `Language: en-GB`); README CRAN install block + `devtools::build_readme()`; add `^plans$` to `.Rbuildignore`. **Commit + `cca`.**

## Verification

- `Rscript -e "devtools::test(reporter = 'check')"` — all tests pass, including new ones.
- `Rscript -e "devtools::document()"` then `Rscript -e "pkgdown::check_pkgdown()"`.
- Full `devtools::check()` — expect 0/0/0 (run where pandoc is available, or `vignettes = FALSE` locally plus CI).
- Manual: `animate("<3p 3 3 3 3 3 | 3p 3 3 3 3 3>", path = tempfile(fileext = ".gif"))` downloads a real GIF; `synchronousSiteswap("(4,4)(2,2)")@symmetry == "symmetrical"`.
- `urlchecker::url_check()` clean.
- `air format .` before each commit.
