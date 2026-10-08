# Plan: jugglr explorer — a Shiny app for the jugglr package

> **Note:** This app is to be built in a **fresh directory** (e.g. `jugglr-app/`), not inside the jugglr package repo. This plan is self-contained — everything needed for setup in the new directory is noted here. Copy this file there when starting.

## Context

jugglr (v0.1.0, GitHub/R-universe only, not CRAN) validates and visualises juggling siteswap patterns. This plan builds a single-file bslib Shiny app that showcases the package: pattern validation + info, `timeline()` and `ladder()` plots, JugglingLab animation, and the `throw_data()` table. Deployment target is **Posit Connect Cloud** (deploys from a GitHub repo via `manifest.json`).

## jugglr API facts (ground truth, verified against source)

- Install: `install.packages("jugglr", repos = c("https://ellakaye.r-universe.dev", getOption("repos")))`. Requires R >= 4.1.0. Imports: cli, dplyr, ggplot2, marquee, purrr, rlang, S7, stringr.
- `siteswap(sequence)` auto-detects notation → S7 object, one of: `vanillaSiteswap`, `synchronousSiteswap`, `multiplexSiteswap`, `synchronousMultiplexSiteswap`, `passingSiteswap`.
- Properties via `@`: `sequence`, `type`, `period`, `symmetry`, `n_props`, `can_throw`, `satisfies_average_theorem`, `valid`; passing adds `is_fractional`, `n_jugglers`.
- **Two-layer validity:** malformed strings throw classed errors (`jugglr_error_not_valid_siteswap`, `jugglr_error_invalid_sequence`, `jugglr_error_not_string`); well-formed but unjuggleable patterns (e.g. `"432"`) do NOT error — they return an object with `@valid == FALSE`, explained by `@satisfies_average_theorem` / `@can_throw`.
- `timeline(ss, n_cycles = 3, title, subtitle)` and `ladder(ss, n_cycles = 3, direction = c("horizontal","vertical"), ...)` return **ggplot objects**; they render fine even for invalid patterns (teaching feature). Passing ladder accepts `hand_gap` (leave at default). They warn if not all props appear in range.
- `throw_data(ss, n_cycles = 3)` returns a data.frame: `beat, hand, throw, catch_beat, catch_hand, prop` (+ `is_crossing` for sync; + `juggler, is_pass, catch_juggler` for passing).
- `animate(pattern, colors, prop, bps, width, height, slowdown, ..., path)`: builds a **remote JugglingLab GIF-server URL** and, when `path` (must end `.gif`) is given, downloads via `utils::download.file`. Without `path` it opens an IDE viewer — unusable in Shiny, so always pass a temp path. Takes several seconds; needs internet to jugglinglab.org at runtime. Throws `jugglr_error_invalid_siteswap` when `@valid` is FALSE. **Fractional passing notation cannot be animated.**
- Example patterns: `"3"`, `"531"`, `"423"`, `"97531"`, `"[54]24"`, `"(4,2x)*"`, `"(4,4)(4x,4x)"`, `"<3p 3|3p 3>"`, `"<4p 3 | 3 4p>"`, fractional `"<4.5 3 3 | 3 4 3.5>"`, invalid demo `"432"`.

## Step 0 — Prerequisites (local)

```r
install.packages(c("shiny", "bslib", "DT", "rsconnect"))
install.packages("jugglr", repos = c("https://ellakaye.r-universe.dev", getOption("repos")))
```

- Install jugglr **from r-universe with `install.packages()`**, not pak/GitHub — the r-universe repo URL gets stamped into the installed DESCRIPTION, which `rsconnect::writeManifest()` records so Connect Cloud can restore it (see Step 6 for fallback).
- **Table package: DT** (over reactable) — fewer dependencies, client-side sort/filter out of the box, tables here are tiny.

## Step 1 — Project scaffold

```
jugglr-app/
├── app.R          # the entire app
├── README.md      # what it is, run + deploy instructions
├── .gitignore     # .Rproj.user, .Rhistory, .RData, *.Rproj, rsconnect/
└── manifest.json  # generated in Step 6, committed
```

No DESCRIPTION or dependencies.R — Connect Cloud reads `manifest.json` for R apps.

```bash
git init && git add . && git commit   # + cca
gh repo create EllaKaye/jugglr-app --public --source=. --push
```

(Connect Cloud deploys from GitHub, so the repo must exist there before publishing.)

## Step 2 — app.R: UI (bslib `page_sidebar`)

Top of file: `library(shiny); library(bslib); library(jugglr); library(DT)` and an `EXAMPLES` named vector of the patterns listed above (including the invalid `"432"` demo and the fractional passing example).

**Layout: `page_sidebar`** — one shared sidebar (pattern and `n_cycles` affect every panel), main area is a `navset_card_tab`. Theme: `bs_theme(version = 5, bootswatch = "flatly")` (one line, easy to swap).

Sidebar:
- `selectInput("example")` with leading `"(custom)"` choice + `textInput("pattern", value = "531")`. Observer: picking an example writes it into the text input via `updateTextInput`; **the text input is the single source of truth**.
- `sliderInput("n_cycles", min = 1, max = 8, value = 3)`.
- `radioButtons("direction", c("horizontal", "vertical"))` — ladder only.
- `accordion(accordion_panel("Animation options", ...))`: `selectInput("colors", ...)` offering `"mixed"`, `"orbits"`, and 2–3 named-colour presets (no free-text hex — scope cut); `selectInput("prop", c("ball", "ring"))` (omit `"image"` — needs a URL); `sliderInput("slowdown", 0.5–4, value = 2, step = 0.5)`. Width/height fixed (~400×450), not exposed.

Main area:
- `uiOutput("status")` at top: green (valid: type, period, props), amber (well-formed but unjuggleable, with explanation), red (unparseable, with error message).
- `navset_card_tab` with five panels: **Info** (property table from the `@` properties, incl. `n_jugglers`/`is_fractional` when passing), **Timeline** (`plotOutput`), **Ladder** (`plotOutput`), **Animation** (`uiOutput` button + `imageOutput("gif")`), **Data** (`DTOutput`).

## Step 3 — app.R: server, two-layer validation

```r
ss <- reactive({
  req(nzchar(trimws(input$pattern)))
  tryCatch(
    list(ok = TRUE, obj = siteswap(trimws(input$pattern))),
    error = \(e) list(ok = FALSE, msg = cli::ansi_strip(conditionMessage(e)))
  )
}) |> debounce(400)
```

- Layer 1: `ss()$ok == FALSE` → red status; panels show `validate(need(...))` placeholders.
- Layer 2: `ok` but `obj@valid == FALSE` → amber status built from `@satisfies_average_theorem` and `@can_throw`; **plots and data still render** (only animation is blocked).
- `debounce(400)` so typing doesn't error keystroke-by-keystroke.

Outputs (straightforward): `renderPlot` for `timeline(ss()$obj, n_cycles = input$n_cycles)` and `ladder(..., direction = input$direction)`; `renderDT(throw_data(...), options = list(pageLength = 15))`. Let the "not all props shown" warning go to the log for v1.

## Step 4 — app.R: animation

**`actionButton` + `withProgress`, not ExtendedTask** — `download.file()` blocks at C level anyway; mirai/promises plumbing is overkill for a demo app (note as future work in a comment).

Key logic:
- `anim_ok()` reactive: `ss()$ok && obj@valid && !(passing && obj@is_fractional)`. Render the button via `uiOutput`; when blocked show *why* (e.g. "Fractional passing patterns can't be animated by JugglingLab").
- `observeEvent(input$animate_btn)`: per-session path `file.path(tempdir(), paste0("jugglr-", session$token, ".gif"))`; `withProgress("Requesting GIF from jugglinglab.org…")` around `tryCatch(animate(obj, colors=, prop=, slowdown=, path=path))`; on error `showNotification`, on success set `gif_path(path)` (a `reactiveVal`).
- `renderImage(..., deleteFile = FALSE)` — with TRUE, a re-render (panel resize) would 404. Clean up in `session$onSessionEnded(\() unlink(path))`. Per-session filename avoids concurrent users clobbering each other.
- Clear `gif_path(NULL)` whenever `ss()` changes so a stale GIF isn't shown for a new pattern.

## Step 5 — Local verification

1. `shiny::runApp()`; walk every EXAMPLES entry: status box, Info panel, timeline/ladder render, Data columns match class (extra cols for sync/passing).
2. `"432"` → amber status with average-theorem explanation; plots still render; animate blocked.
3. Garbage (`"abc!"`, `""`) → red status, no crashed outputs.
4. Fractional `"<4.5 3 3 | 3 4 3.5>"` → all works except animation, with explanation.
5. Animate `"531"` and `"(4,2x)*"` → GIF appears after progress; change pattern → GIF clears; regenerate works. Test colour presets and `prop = "ring"`.
6. `n_cycles` extremes (1, 8) on `"97531"`.

## Step 6 — Posit Connect Cloud deployment

1. `rsconnect::writeManifest()` in the project dir (jugglr installed from r-universe per Step 0).
2. **Verify manifest before committing:** the `"jugglr"` entry's Repository/Source field must point at `https://ellakaye.r-universe.dev`.
3. **Fallback** if the source is missing/`unknown`: `remotes::install_github("EllaKaye/jugglr")` (stamps `RemoteType: github` + SHA into DESCRIPTION, which the manifest carries), regenerate, re-verify.
4. Commit `manifest.json`, push.
5. connect.posit.cloud → New content → Shiny (R) → select repo/branch → primary file `app.R` → publish.
6. Post-deploy smoke test, **especially animation** — Connect Cloud needs outbound access to jugglinglab.org; if the GIF fails there but works locally, that's the cause (the tryCatch surfaces it as a notification rather than a crash).
7. README note: regenerate + recommit `manifest.json` whenever package versions change.

## Commit points (each followed by `cca`)

1. Scaffold: .gitignore, README — after Step 1.
2. App UI + validation + plots + data table — after Steps 2–3 verified locally.
3. Animation panel with guards and cleanup — after Step 4.
4. Polish from Step 5 testing (theme, status box, edge cases).
5. manifest.json for Connect Cloud — after Step 6 verification.
6. Post-deploy: README live-app link.

## Deliberate scope cuts

- Fixed animation width/height; no `prop = "image"`; no free-form hex colours; no `hand_gap` UI.
- `withProgress` over ExtendedTask (blocking download; documented as future work).
- DT over reactable; `page_sidebar` over `page_navbar` (shared inputs).
