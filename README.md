
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jugglr <a href="https://ellakaye.github.io/jugglr/"><img src="man/figures/logo.png" align="right" height="138" alt="jugglr website" /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/EllaKaye/jugglr/graph/badge.svg)](https://app.codecov.io/gh/EllaKaye/jugglr)
[![R-CMD-check](https://github.com/EllaKaye/jugglr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EllaKaye/jugglr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**jugglr** is an R package for validating and visualising juggling
patterns expressed in [siteswap
notation](https://en.wikipedia.org/wiki/Siteswap). The `siteswap()`
function takes a siteswap sequence and creates an S7 object, with child
classes for vanilla, synchronous, multiplex, synchronous multiplex, and
passing siteswap.

## Installation

You can install the development version of jugglr from
[GitHub](https://github.com/EllaKaye/jugglr) with:

``` r
# install.packages("pak")
pak::pak("EllaKaye/jugglr")
```

## Siteswap

Once a Siteswap object is created, its print method will display
information about the sequence, such as whether it is a valid juggling
pattern and, if so, how many props it uses.

``` r
library(jugglr)
# A valid juggling pattern
ss423 <- siteswap("423")
ss423
#> ✔ '423' is valid vanilla siteswap
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 3
```

``` r
# A pattern that cannot be juggled
ss21 <- siteswap("21")
ss21
#> ✖ '21' is not a valid juggling pattern
#> ℹ The throws don't average to a whole number
#> ℹ Two or more throws land on the same beat (collision)
```

## Visualising the patterns

There are three ways of visualising the siteswap patterns:

- A timeline plot, which shows the throws by beat
- A ladder diagram, which shows the throws by beat and hand
- An animation (only if the pattern is valid)

### Plots

These functions take `Siteswap` objects as their argument and return
ggplot2 plots, so can be further customised.

``` r
timeline(ss423)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" />

``` r
ladder(ss423)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" alt="" width="100%" />

These plots are also useful for understanding why non-valid sequences
are not jugglable. We can see, for example, where two props would need
to be caught at the same time (which is not permissible in vanilla
siteswap). Because each prop is shown in a different colour, we can see
where balls are disappearing or needing suddenly to appear.

``` r
timeline(ss21)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" alt="" width="100%" />

``` r
ladder(ss21)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" alt="" width="100%" />

### Animation

**jugglr** provides a wrapper to the [JugglingLab GIF
server](https://jugglinglab.org/html/animinfo.html). Unlike the plotting
functions, the main argument is any *valid* siteswap sequence as a
string.

If called in Positron or RStudio, `animate()` will show the animation in
the Viewer pane, otherwise in the browser. If a `path` argument is
supplied, the animation will be saved to that location instead. Note
that it can take several seconds for the animation to render.

To save a GIF for embedding in an R Markdown or Quarto document, pass a
`path` argument to `animate()`, then reference the saved file with
`knitr::include_graphics()`. Display options can be set as chunk
arguments:

``` r
animate("423", path = "man/figures/423-animation.gif", colors = c("#E69F00", "#56B4E9", "#009E73"))
```

<img src="man/figures/423-animation.gif" alt="" width="40%" />
