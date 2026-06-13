# Plot a ladder diagram for a siteswap

Produces a ladder diagram showing prop paths between hands across beats.
Straight lines indicate self-throws (same hand catches); crossing lines
indicate cross-throws (opposite hand catches).

## Usage

``` r
ladder(
  siteswap,
  n_cycles = 3,
  direction = c("horizontal", "vertical", "h", "v"),
  title = TRUE,
  subtitle = TRUE,
  ...
)
```

## Arguments

- siteswap:

  Any siteswap object: a `vanillaSiteswap`, `synchronousSiteswap`,
  `multiplexSiteswap`, `synchronousMultiplexSiteswap`, or
  `passingSiteswap`.

- n_cycles:

  Number of complete cycles to simulate (default 3).

- direction:

  Orientation of the diagram: `"horizontal"` (default, time runs left to
  right) or `"vertical"` (time runs top to bottom). Shorthands `"h"` and
  `"v"` are also accepted.

- title:

  Logical. If `TRUE` (default), adds a title showing the siteswap
  sequence. Set to `FALSE` to suppress; override with
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
  on the returned plot.

- subtitle:

  Logical. If `TRUE` (default), adds a subtitle showing the siteswap
  type and number of props. Set to `FALSE` to suppress; override with
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
  on the returned plot.

- ...:

  Additional arguments passed to methods. For `passingSiteswap` objects,
  `hand_gap` (default `2`) sets the vertical spacing between each
  juggler's two hands in the diagram.

## Value

A ggplot2 object.

## Examples

``` r
s <- vanillaSiteswap("531")
ladder(s)

```
