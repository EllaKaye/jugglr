# jugglr (development version)

First release. jugglr validates and visualises juggling patterns written in siteswap notation.

* `siteswap()` detects the notation style of a sequence and returns a typed S7 object: `vanillaSiteswap()`, `synchronousSiteswap()`, `multiplexSiteswap()`, `synchronousMultiplexSiteswap()`, or `passingSiteswap()`. Each constructor is also exported for direct use. Passing siteswap supports both p-notation and fractional notation.
* Printing a siteswap object reports whether the pattern is valid, and, for valid patterns, the number of props, period, and symmetry. Invalid patterns report why (average theorem or collisions).
* `timeline()` draws an arc diagram and `ladder()` draws a ladder diagram, both for every siteswap type and both returning ggplot2 objects.
* `throw_data()` returns the underlying throw-by-throw data frame for custom visualisations.
* `animate()` produces an animated GIF of a pattern via the JugglingLab animation server, with options for colours, prop type, and speed.
