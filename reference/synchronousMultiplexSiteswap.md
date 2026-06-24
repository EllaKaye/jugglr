# Synchronous multiplex siteswap

Creates a synchronous multiplex siteswap object. This combines
synchronous notation (both hands throw simultaneously, written as pairs
in parentheses) with multiplex notation (square brackets group
simultaneous throws from the same hand). For example,
`"(2,4)([4x4],2x)"` is a 4-prop pattern where the second slot has hand 0
throwing two balls simultaneously.

## Usage

``` r
synchronousMultiplexSiteswap(sequence = character(0))
```

## Arguments

- sequence:

  A single character string of synchronous multiplex siteswap notation,
  e.g. `"(2,4)([4x4],2x)"`.

## Value

A `synchronousMultiplexSiteswap` S7 object.

## Details

All individual throw heights must be even. An `x` suffix marks a
crossing throw; a trailing `*` indicates the pattern alternates between
two mirrored versions.

## See also

The visualisation functions
[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md),
[`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md) and
[`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md).

Other siteswap constructors:
[`multiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md),
[`passingSiteswap()`](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md),
[`siteswap()`](https://ellakaye.github.io/jugglr/reference/siteswap.md),
[`synchronousSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md),
[`vanillaSiteswap()`](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)

## Additional properties

- `@type`:

  Always `"synchronous multiplex"` (read-only).

- `@full_sequence`:

  The expanded sequence with the `*` shorthand resolved (read-only).

- `@throws`:

  Character vector of all slot throw strings across one expanded cycle.
  Multiplex groups are kept as single elements, e.g. `"[4x4]"`
  (read-only).

- `@throws_by_hand`:

  Named list with elements `hand_1` and `hand_2`, each a character
  vector of throw strings per slot (one per sync slot) (read-only).

- `@period`:

  Number of throw slots per full cycle (2 × number of sync slots, since
  both hands throw on every beat; always even) (read-only).

- `@symmetry`:

  `"symmetrical"` if the pattern is its own mirror image;
  `"asymmetrical"` otherwise (read-only).

- `@n_props`:

  Number of props: sum of all throw heights divided by the period
  (read-only).

- `@can_throw`:

  `TRUE` if thrown and landing prop counts balance at every (slot, hand)
  pair within one cycle (read-only).

- `@satisfies_average_theorem`:

  `TRUE` if `n_props` is a whole number (read-only).

- `@valid`:

  `TRUE` if both `can_throw` and `satisfies_average_theorem` are `TRUE`
  (read-only).

## Examples

``` r
synchronousMultiplexSiteswap("(2,4)([4x4],2x)")
#> ✔ '(2,4)([4x4],2x)' is valid synchronous multiplex siteswap
#> ℹ It uses 4 props
#> ℹ It is asymmetrical with period 4
synchronousMultiplexSiteswap("(4,[42x])*")
#> ✔ '(4,[42x])*' is valid synchronous multiplex siteswap
#> ℹ Full sequence: (4,[42x])([42x],4)
#> ℹ It uses 5 props
#> ℹ It is symmetrical with period 4

s <- synchronousMultiplexSiteswap("(2,4)([4x4],2x)")
s@n_props
#> [1] 4
s@valid
#> [1] TRUE
```
