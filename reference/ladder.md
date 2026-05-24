# Plot a ladder diagram for a siteswap

Produces a ladder diagram showing prop paths between hands across beats.
Straight lines indicate self-throws (same hand catches); crossing lines
indicate cross-throws (opposite hand catches).

## Usage

``` r
ladder(siteswap, n_cycles = 3, direction = c("horizontal", "vertical"), ...)
```

## Arguments

- siteswap:

  A `vanillaSiteswap`, `synchronousSiteswap`, or `multiplexSiteswap`
  object.

- n_cycles:

  Number of complete cycles to simulate (default 3).

- direction:

  Orientation of the diagram: `"horizontal"` (default, time runs left to
  right) or `"vertical"` (time runs top to bottom).

- ...:

  Additional arguments passed to methods.

## Value

A ggplot2 object.
