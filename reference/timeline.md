# Plot a timeline diagram for a siteswap

Produces a timeline (arc) diagram showing the trajectory of each prop
across beats. Each arc represents a throw, coloured by prop. The x-axis
labels show the throw heights from the siteswap sequence.

## Usage

``` r
timeline(siteswap, ...)
```

## Arguments

- siteswap:

  A `vanillaSiteswap` or `synchronousSiteswap` object.

- ...:

  Additional arguments passed to methods.

- n_cycles:

  Number of complete cycles to simulate (default 3). A warning is issued
  if not all props appear within the simulated range.

- title:

  Logical. If `TRUE` (default), adds a title and subtitle showing the
  sequence and number of props. Set to `FALSE` to suppress; override
  with
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
  on the returned plot.

## Value

A ggplot2 object.
