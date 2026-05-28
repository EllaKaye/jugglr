# Get throw data for a siteswap

Returns a data frame describing each throw in the simulated pattern,
including which beat and hand each throw originates from, where it
lands, and which prop is thrown. This underpins
[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
and [`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md),
and can be used directly for custom visualisations.

## Usage

``` r
throw_data(siteswap, n_cycles = 3, ...)
```

## Arguments

- siteswap:

  A `vanillaSiteswap`, `synchronousSiteswap`, or `multiplexSiteswap`
  object.

- n_cycles:

  Number of complete cycles to simulate (default 3). Increase for
  patterns with many props to ensure all appear in the data. Setting
  `n_cycles >= period * n_props * 2` guarantees each prop is thrown at
  least twice.

- ...:

  Additional arguments passed to methods.

## Value

A data frame with one row per throw and columns:

- `beat`: throw beat index

- `hand`: throwing hand (0 or 1)

- `throw`: throw height (integer)

- `catch_beat`: beat on which the prop lands

- `catch_hand`: hand that catches (0 or 1)

- `prop`: prop identifier (integer)

## Examples

``` r
s <- vanillaSiteswap("531")
throw_data(s)
#>   beat hand throw catch_beat catch_hand prop
#> 1    1    0     5          6          1    1
#> 2    2    1     3          5          0    2
#> 3    3    0     1          4          1    3
#> 4    4    1     5          9          0    3
#> 5    5    0     3          8          1    2
#> 6    6    1     1          7          0    1
#> 7    7    0     5         12          1    1
#> 8    8    1     3         11          0    2
#> 9    9    0     1         10          1    3
```
