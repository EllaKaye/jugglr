# Synchronous siteswap

Creates a synchronous siteswap object from a two-handed
simultaneous-throw sequence. Synchronous notation describes patterns
where both hands throw at the same time, written as pairs of throw
heights in parentheses such as `"(4,4)"` or `"(4,2x)*"`. An `x` suffix
marks a crossing throw; a trailing `*` indicates the pattern alternates
between two mirrored versions.

## Usage

``` r
synchronousSiteswap(sequence = character(0))
```

## Arguments

- sequence:

  A single character string of synchronous siteswap notation, e.g.
  `"(4,4)"` or `"(4,2x)*"`.

## Value

A `synchronousSiteswap` S7 object.

## See also

The visualisation functions
[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md),
[`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md) and
[`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md).

Other siteswap constructors:
[`multiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md),
[`passingSiteswap()`](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md),
[`siteswap()`](https://ellakaye.github.io/jugglr/reference/siteswap.md),
[`synchronousMultiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousMultiplexSiteswap.md),
[`vanillaSiteswap()`](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)

## Additional properties

- `@type`:

  Always `"synchronous"` (read-only).

- `@full_sequence`:

  The expanded sequence with the `*` shorthand resolved (read-only).

- `@throws`:

  Character vector of all individual throw values across one expanded
  cycle (read-only).

- `@throws_by_hand`:

  Named list with elements `hand_1` and `hand_2`, each a character
  vector of throws for that hand per slot (read-only).

- `@period`:

  Number of throw slots per full cycle (counts both hands per
  simultaneous beat, so always even) (read-only).

- `@symmetry`:

  `"symmetrical"` if the pattern is its own mirror image;
  `"asymmetrical"` otherwise (read-only).

- `@n_props`:

  Mean of the slide sequence, equal to the number of props (read-only).

- `@can_throw`:

  `TRUE` if no collisions occur in the slide sequence (read-only).

- `@satisfies_average_theorem`:

  `TRUE` if `n_props` is a whole number (read-only).

- `@valid`:

  `TRUE` if both `can_throw` and `satisfies_average_theorem` are `TRUE`
  (read-only).

## Examples

``` r
synchronousSiteswap("(4,4)")
#> ✔ '(4,4)' is valid synchronous siteswap
#> ℹ It uses 4 props
#> ℹ It is symmetrical with period 2
synchronousSiteswap("(4,2x)*")
#> ✔ '(4,2x)*' is valid synchronous siteswap
#> ℹ Full sequence: (4,2x)(2x,4)
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 4

s <- synchronousSiteswap("(4,2x)*")
s@n_props
#> [1] 3
s@valid
#> [1] TRUE
```
