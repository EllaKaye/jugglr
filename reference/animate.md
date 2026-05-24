# Embed juggling animation in R Markdown or Quarto

Embed juggling animation in R Markdown or Quarto

## Usage

``` r
animate(
  pattern,
  colors = NULL,
  bps = NULL,
  width = NULL,
  height = NULL,
  fps = NULL,
  slowdown = NULL,
  ...,
  path = NULL
)

animate_markdown(pattern, path, colors = NULL, ...)
```

## Arguments

- pattern:

  Siteswap pattern

- colors:

  Optional vector of colors

- ...:

  Additional arguments to JugglingLab GIT server

- path:

  Path to save GIF. If `NULL` (default) view in viewer

## Value

knitr graphics object for embedding
