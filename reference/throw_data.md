# Get throw data for a siteswap

Returns a data frame describing each throw in the simulated pattern,
including which beat and hand each throw originates from, where it
lands, and which prop is thrown. This underpins
[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
and [`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md),
and can be used directly for custom visualisations.

## Usage

``` r
throw_data(siteswap, ...)
```

## Arguments

- siteswap:

  A `vanillaSiteswap` or `synchronousSiteswap` object.

- ...:

  Additional arguments passed to methods.

- n_cycles:

  Number of complete cycles to simulate (default 3). Increase for
  patterns with many props to ensure all appear in the data. Setting
  `n_cycles >= period * n_props * 2` guarantees each prop is thrown at
  least twice.

## Value

A data frame with one row per throw and columns:

- `beat`: throw beat index

- `hand`: throwing hand (0 or 1)

- `throw`: throw height (integer)

- `catch_beat`: beat on which the prop lands

- `catch_hand`: hand that catches (0 or 1)

- `prop`: prop identifier (integer)
