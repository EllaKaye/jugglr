# Passing siteswap

Creates a passing siteswap object from a multi-juggler sequence in
`<A|B>` notation. Each section separated by `|` gives one juggler's
throws; a `p` suffix (e.g. `"3p"`) marks a throw that passes to the next
juggler. Fractional notation (e.g. `"<4.5 3 3 | 3 4 3.5>"`) is also
supported, where `.5` throws are passes and the second juggler is half a
beat later.

## Usage

``` r
passingSiteswap(sequence = character(0))
```

## Arguments

- sequence:

  A single character string of passing siteswap notation, e.g.
  `"<3p 3 3 3 3 3 | 3p 3 3 3 3 3>"` or `"<4.5 3 3 | 3 4 3.5>"`. Spaces
  within sections are ignored; `<3p33|3p33>` and `<3p 3 3|3p 3 3>` are
  equivalent.

## Value

A `passingSiteswap` S7 object.

## Details

In p-notation, both jugglers are fully synchronised: they throw on the
same beat and alternate hands together. In fractional notation, the
second juggler is offset by half a beat in the combined timeline.

## See also

The visualisation functions
[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md),
[`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md) and
[`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md).

Other siteswap constructors:
[`multiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md),
[`siteswap()`](https://ellakaye.github.io/jugglr/reference/siteswap.md),
[`synchronousMultiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousMultiplexSiteswap.md),
[`synchronousSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md),
[`vanillaSiteswap()`](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)

## Additional properties

- `@type`:

  Always `"passing"` (read-only).

- `@is_fractional`:

  `TRUE` for fractional notation; `FALSE` for p-notation (read-only).

- `@n_jugglers`:

  Number of jugglers (sections in the sequence) (read-only).

- `@sequences_by_juggler`:

  List of character vectors, one per juggler, each giving the raw tokens
  (e.g. `c("3p", "3", "3")`) (read-only).

- `@throws_by_juggler`:

  List of numeric vectors, one per juggler, of throw heights (integers
  for p-notation; may include `.5` for fractional) (read-only).

- `@is_pass_by_juggler`:

  List of logical vectors, one per juggler, indicating which throws are
  passes (read-only).

- `@period`:

  Number of throws per juggler per cycle (read-only).

- `@symmetry`:

  `"symmetrical"` when period is odd; `"asymmetrical"` when period is
  even (read-only).

- `@n_props`:

  Total number of props (`sum(all throws) / period`) (read-only).

- `@can_throw`:

  `TRUE` if no two throws land on the same `(juggler, beat, hand)`
  triple (no collision) (read-only).

- `@satisfies_average_theorem`:

  `TRUE` if `n_props` is a whole number (read-only).

- `@valid`:

  `TRUE` if both `can_throw` and `satisfies_average_theorem` are `TRUE`
  (read-only).

## Examples

``` r
passingSiteswap("<3p 3 3 3 3 3 | 3p 3 3 3 3 3>")
#> ✔ '<3p 3 3 3 3 3 | 3p 3 3 3 3 3>' is valid passing siteswap
#> ℹ It uses 6 props across 2 jugglers
#> ℹ It is asymmetrical with period 6
passingSiteswap("<4p 3 | 3 4p>")
#> ✔ '<4p 3 | 3 4p>' is valid passing siteswap
#> ℹ It uses 7 props across 2 jugglers
#> ℹ It is asymmetrical with period 2

# Compact and spaced forms are equivalent
s1 <- passingSiteswap("<3p33|3p33>")
s2 <- passingSiteswap("<3p 3 3 | 3p 3 3>")
identical(s1@throws_by_juggler, s2@throws_by_juggler)
#> [1] TRUE

s <- passingSiteswap("<4p 3 | 3 4p>")
s@n_props
#> [1] 7
s@valid
#> [1] TRUE
```
