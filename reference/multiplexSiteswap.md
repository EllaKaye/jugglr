# Multiplex siteswap

Creates a multiplex siteswap object from a sequence in which square
brackets group simultaneous throws from the same hand. For example,
`"[43]1"` means the juggler throws heights 4 and 3 simultaneously on the
first beat, then a single 1 on the second beat. Any slot without
brackets is a single throw exactly as in vanilla siteswap.

## Usage

``` r
multiplexSiteswap(sequence = character(0))
```

## Arguments

- sequence:

  A single character string of multiplex siteswap notation, e.g.
  `"[43]1"` or `"[33]"`.

## Value

A `multiplexSiteswap` S7 object.

## See also

The visualisation functions
[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md),
[`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md) and
[`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md).

Other siteswap constructors:
[`passingSiteswap()`](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md),
[`siteswap()`](https://ellakaye.github.io/jugglr/reference/siteswap.md),
[`synchronousMultiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousMultiplexSiteswap.md),
[`synchronousSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md),
[`vanillaSiteswap()`](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)

## Additional properties

- `@type`:

  Always `"multiplex"` (read-only).

- `@slots`:

  List of integer vectors, one element per time slot. Single-throw slots
  are length-1 vectors; multiplex slots have length \> 1 (read-only).

- `@throws`:

  Integer vector of all individual throw heights (flattened across all
  slots) (read-only).

- `@period`:

  Number of time slots per cycle (read-only).

- `@symmetry`:

  `"symmetrical"` when period is odd; `"asymmetrical"` when period is
  even (read-only).

- `@n_props`:

  Total of all throw heights divided by the period — the number of props
  required. (Note: `sum(throws) / period`, not `mean(throws)`.)
  (read-only).

- `@can_throw`:

  `TRUE` if for every slot the number of props thrown equals the number
  of props landing (no conservation violation) (read-only).

- `@satisfies_average_theorem`:

  `TRUE` if `n_props` is a whole number (read-only).

- `@valid`:

  `TRUE` if both `can_throw` and `satisfies_average_theorem` are `TRUE`
  (read-only).

## Examples

``` r
multiplexSiteswap("[43]1")
#> ✔ '[43]1' is valid multiplex siteswap
#> ℹ It uses 4 props
#> ℹ It is asymmetrical with period 2
multiplexSiteswap("[33]")
#> ✔ '[33]' is valid multiplex siteswap
#> ℹ It uses 6 props
#> ℹ It is symmetrical with period 1

s <- multiplexSiteswap("[43]1")
s@n_props
#> [1] 4
s@valid
#> [1] TRUE
```
