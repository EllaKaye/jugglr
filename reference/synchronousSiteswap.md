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

## Additional properties

- `@type`:

  Always `"synchronous"` (read-only).

- `@full_sequence`:

  The expanded sequence with the `*` shorthand resolved.

- `@throws`:

  Character vector of all individual throw values across one expanded
  cycle.

- `@throws_by_hand`:

  Named list with elements `hand_1` and `hand_2`, each a character
  vector of throws for that hand per slot.

- `@period`:

  Number of throw slots per full cycle (counts both hands per
  simultaneous beat, so always even).

- `@symmetry`:

  `"symmetrical"` if the pattern is its own mirror image;
  `"asymmetrical"` otherwise.

- `@n_props`:

  Mean of the slide sequence, equal to the number of props.

- `@can_throw`:

  `TRUE` if no collisions occur in the slide sequence.

- `@satisfies_average_theorem`:

  `TRUE` if `n_props` is a whole number.

- `@valid`:

  `TRUE` if both `can_throw` and `satisfies_average_theorem` are `TRUE`.

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
