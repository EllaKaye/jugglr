# Animate a juggling pattern

Generates an animated GIF of a juggling pattern via the [JugglingLab GIF
server](https://jugglinglab.org/html/animinfo.html) and displays it in
the RStudio viewer (or default browser). If `path` is given the GIF is
saved to disk instead.

## Usage

``` r
animate(
  pattern,
  colors = NULL,
  bps = NULL,
  width = NULL,
  height = NULL,
  fps = NULL,
  slowdown = NULL,
  ...,
  path = NULL
)

animate_markdown(pattern, path, colors = NULL, ...)
```

## Arguments

- pattern:

  A siteswap pattern string (e.g. `"531"`) or a siteswap object.

- colors:

  Optional. A vector of R colours (one per prop), or one of the special
  strings `"mixed"` or `"orbits"`. Passed to JugglingLab.

- bps:

  Beats per second (numeric scalar). Controls the animation speed.

- width, height:

  Width and height of the animation in pixels (numeric scalars).

- fps:

  Frames per second (numeric scalar).

- slowdown:

  Slowdown factor (numeric scalar). Values greater than 1 slow the
  animation; values less than 1 speed it up.

- ...:

  Additional named arguments passed to the JugglingLab GIF server.
  Pattern-setting arguments include `dwell`, `hands`, `body`,
  `propdiam`, `prop`, `gravity`, `bouncefrac`, `squeezebeats`, `hss`,
  `handspec`, `dwellmax`, and `hold`. Animation arguments include
  `stereo`, `border`, `camangle`, `showground`, and `hidejugglers`. All
  values must be scalars.

- path:

  Path to save the GIF (must end in `.gif`). If `NULL` (default) the
  animation is displayed in the viewer.

## Value

Invisibly returns the path to the temporary HTML file (when displaying)
or the save path (when `path` is given).

A knitr graphics object for inline embedding.

## Details

Note that setting `colors` can introduce a short delay while the server
renders the animation.

## Functions

- `animate_markdown()`: Save a GIF to `path` and embed it in an R
  Markdown or Quarto document via
  [`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html).
  The `path` argument is required; set a persistent file path (not a
  [`tempfile()`](https://rdrr.io/r/base/tempfile.html)) so the rendered
  document can reference it.
