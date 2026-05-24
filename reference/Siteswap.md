# Create a siteswap object

Creates a typed siteswap object by detecting the notation style of
`sequence`. Vanilla siteswaps (alphanumeric strings such as `"531"`)
produce a
[vanillaSiteswap](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)
object; synchronous siteswaps in `(a,b)` notation such as `"(4,2x)*"`
produce a
[synchronousSiteswap](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md)
object.

## Usage

``` r
siteswap(sequence)
```

## Arguments

- sequence:

  A single character string of siteswap notation.

## Value

A
[vanillaSiteswap](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)
or
[synchronousSiteswap](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md)
S7 object.

## Examples

``` r
siteswap("531")
#> ✔ '531' is valid vanilla siteswap
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 3
siteswap("(4,2x)*")
#> ✔ '(4,2x)*' is valid synchronous siteswap
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 4
```
