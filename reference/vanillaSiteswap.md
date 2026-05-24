# Vanilla siteswap

Creates a vanilla siteswap object from an alphanumeric siteswap sequence
such as `"531"` or `"97531"`. Vanilla siteswap describes solo juggling
patterns where one prop is thrown per beat, alternating hands.

## Usage

``` r
vanillaSiteswap(sequence = character(0))
```

## Arguments

- sequence:

  A single character string of vanilla siteswap notation (digits and
  letters only, e.g. `"531"`).

## Value

A `vanillaSiteswap` S7 object.

## Additional properties

- `@type`:

  Always `"vanilla"` (read-only).

- `@throws`:

  Integer vector of throw heights for one cycle.

- `@period`:

  Number of throws per cycle (length of `throws`).

- `@symmetry`:

  `"symmetrical"` when period is odd (pattern repeats with swapped
  hands); `"asymmetrical"` when period is even.

- `@n_props`:

  Mean throw height, equal to the number of props required.

- `@can_throw`:

  `TRUE` if no two throws land on the same beat (no collisions).

- `@satisfies_average_theorem`:

  `TRUE` if `n_props` is a whole number.

- `@valid`:

  `TRUE` if both `can_throw` and `satisfies_average_theorem` are `TRUE`.

## Examples

``` r
vanillaSiteswap("531")
#> ✔ '531' is valid vanilla siteswap
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 3
vanillaSiteswap("97531")
#> ✔ '97531' is valid vanilla siteswap
#> ℹ It uses 5 props
#> ℹ It is symmetrical with period 5

s <- vanillaSiteswap("531")
s@n_props
#> [1] 3
s@valid
#> [1] TRUE
```
