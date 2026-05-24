# Changelog

## jugglr 0.0.0.9000

- [`animate()`](https://ellakaye.github.io/jugglr/reference/animate.md)
  and
  [`animate_markdown()`](https://ellakaye.github.io/jugglr/reference/animate.md)
  animate juggling patterns via the JugglingLab GIF server.
- [`animate()`](https://ellakaye.github.io/jugglr/reference/animate.md)
  gains a `prop` argument for selecting the prop type (`"ball"`,
  `"ring"`, or `"image"`).
- [`animate()`](https://ellakaye.github.io/jugglr/reference/animate.md)
  and
  [`animate_markdown()`](https://ellakaye.github.io/jugglr/reference/animate.md)
  now accept \[vanillaSiteswap\], \[synchronousSiteswap\], and
  \[multiplexSiteswap\] objects as `pattern`, in addition to plain
  strings.
- [`animate_markdown()`](https://ellakaye.github.io/jugglr/reference/animate.md)
  is now exported.
- [`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md)
  plots ladder diagrams for vanilla, synchronous, and multiplex
  siteswaps.
- [`multiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md)
  creates a multiplex siteswap S7 object for patterns with simultaneous
  throws from one hand, written using square-bracket notation such as
  `"[43]1"`.
- [`siteswap()`](https://ellakaye.github.io/jugglr/reference/Siteswap.md)
  creates a vanilla, synchronous, or multiplex siteswap object from a
  notation string.
- [`synchronousSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md)
  creates a synchronous siteswap S7 object.
- [`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md)
  returns the raw throw data frame underlying the visualisation
  functions, for use in custom visualisations.
- [`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
  plots arc/timeline diagrams for vanilla and multiplex siteswaps.
- [`vanillaSiteswap()`](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)
  creates a vanilla siteswap S7 object.
