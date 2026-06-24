# Create a siteswap object

Creates a typed siteswap object by detecting the notation style of
`sequence`. Vanilla siteswaps (alphanumeric strings such as `"531"`)
produce a
[vanillaSiteswap](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)
object; synchronous siteswaps in `(a,b)` notation such as `"(4,2x)*"`
produce a
[synchronousSiteswap](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md)
object; multiplex siteswaps with square-bracket groups such as `"[43]1"`
produce a
[multiplexSiteswap](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md)
object; passing siteswaps in `<A|B>` notation such as
`"<3p 3 3 3 3 3 | 3p 3 3 3 3 3>"` produce a
[passingSiteswap](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md)
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
[vanillaSiteswap](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md),
[synchronousSiteswap](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md),
[multiplexSiteswap](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md),
[synchronousMultiplexSiteswap](https://ellakaye.github.io/jugglr/reference/synchronousMultiplexSiteswap.md),
or
[passingSiteswap](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md)
S7 object.

## See also

The visualisation functions
[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md),
[`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md) and
[`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md).

Other siteswap constructors:
[`multiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md),
[`passingSiteswap()`](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md),
[`synchronousMultiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousMultiplexSiteswap.md),
[`synchronousSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md),
[`vanillaSiteswap()`](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)

## Examples

``` r
siteswap("531")
#> ✔ '531' is valid vanilla siteswap
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 3
siteswap("(4,2x)*")
#> ✔ '(4,2x)*' is valid synchronous siteswap
#> ℹ Full sequence: (4,2x)(2x,4)
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 4
siteswap("[43]1")
#> ✔ '[43]1' is valid multiplex siteswap
#> ℹ It uses 4 props
#> ℹ It is asymmetrical with period 2
siteswap("(2,4)([4x4],2x)")
#> ✔ '(2,4)([4x4],2x)' is valid synchronous multiplex siteswap
#> ℹ It uses 4 props
#> ℹ It is asymmetrical with period 4
siteswap("<3p 3 3 3 3 3 | 3p 3 3 3 3 3>")
#> ✔ '<3p 3 3 3 3 3 | 3p 3 3 3 3 3>' is valid passing siteswap
#> ℹ It uses 6 props across 2 jugglers
#> ℹ It is asymmetrical with period 6
```
