# Changelog

## jugglr (development version)

First release. jugglr validates and visualises juggling patterns written
in siteswap notation.

- [`siteswap()`](https://ellakaye.github.io/jugglr/reference/siteswap.md)
  detects the notation style of a sequence and returns a typed S7
  object:
  [`vanillaSiteswap()`](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md),
  [`synchronousSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md),
  [`multiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md),
  [`synchronousMultiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousMultiplexSiteswap.md),
  or
  [`passingSiteswap()`](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md).
  Each constructor is also exported for direct use. Passing siteswap
  supports both p-notation and fractional notation.
- Printing a siteswap object reports whether the pattern is valid, and,
  for valid patterns, the number of props, period, and symmetry. Invalid
  patterns report why (average theorem or collisions).
- [`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
  draws an arc diagram and
  [`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md)
  draws a ladder diagram, both for every siteswap type and both
  returning ggplot2 objects.
- [`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md)
  returns the underlying throw-by-throw data frame for custom
  visualisations.
- [`animate()`](https://ellakaye.github.io/jugglr/reference/animate.md)
  produces an animated GIF of a pattern via the JugglingLab animation
  server, with options for colours, prop type, and speed.
