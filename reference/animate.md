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
  prop = NULL,
  bps = NULL,
  width = NULL,
  height = NULL,
  fps = NULL,
  slowdown = NULL,
  ...,
  path = NULL
)
```

## Arguments

- pattern:

  A siteswap pattern string (e.g. `"531"`) or any siteswap object:
  [vanillaSiteswap](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md),
  [synchronousSiteswap](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md),
  [multiplexSiteswap](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md),
  or
  [synchronousMultiplexSiteswap](https://ellakaye.github.io/jugglr/reference/synchronousMultiplexSiteswap.md).
  [passingSiteswap](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md)
  objects are also accepted but their sequence format (`<A|B>`) is not
  valid JugglingLab notation.

- colors:

  Optional. A vector of R colours (one per prop), or one of the special
  strings `"mixed"` or `"orbits"`. Passed to JugglingLab.

- prop:

  Prop type: `"ball"` (default), `"ring"`, or `"image"`.

- bps:

  Beats per second (numeric scalar). Controls the animation speed.

- width, height:

  Width and height of the animation in pixels (numeric scalars).

- fps:

  Frames per second (numeric scalar).

- slowdown:

  Slowdown factor (numeric scalar). The JugglingLab default is `2.0`;
  values greater than this slow the animation further, values less than
  `2.0` speed it up.

- ...:

  Additional named arguments passed to the JugglingLab GIF server.
  Pattern-setting arguments include `dwell`, `hands`, `body`,
  `propdiam`, `gravity`, `bouncefrac`, `squeezebeats`, `hss`,
  `handspec`, `dwellmax`, and `hold`. Animation arguments include
  `stereo`, `border`, `camangle`, `showground`, and `hidejugglers`. All
  values must be scalars.

- path:

  Path to save the GIF (must end in `.gif`). If `NULL` (default) the
  animation is displayed in the viewer.

## Value

Invisibly returns the path to the temporary HTML file (when displaying)
or the save path (when `path` is given).

## Details

Note that setting `colors` can introduce a short delay while the server
renders the animation.

## Examples

``` r
if (FALSE) { # \dontrun{
animate("531")
animate(vanillaSiteswap("531"), prop = "ring", bps = 5)
animate("531", path = tempfile(fileext = ".gif"))
} # }
```
