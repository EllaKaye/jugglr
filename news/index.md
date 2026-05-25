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
  now works for `passingSiteswap` objects, drawing a multi-row diagram
  with passes shown as diagonal segments between juggler rows.
- [`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md)
  plots ladder diagrams for vanilla, synchronous, and multiplex
  siteswaps.
- [`multiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/multiplexSiteswap.md)
  creates a multiplex siteswap S7 object for patterns with simultaneous
  throws from one hand, written using square-bracket notation such as
  `"[43]1"`.
- [`passingSiteswap()`](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md)
  creates a passing siteswap S7 object for multi-juggler patterns in
  `<A|B>` notation. Supports both p-notation (e.g. `"<4p 3 | 3 4p>"`)
  and fractional notation (e.g. `"<4.5 3 3 | 3 4 3.5>"`). Includes
  [`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md),
  [`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md),
  and
  [`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md)
  methods.
- [`siteswap()`](https://ellakaye.github.io/jugglr/reference/Siteswap.md)
  now also dispatches to
  [`passingSiteswap()`](https://ellakaye.github.io/jugglr/reference/passingSiteswap.md)
  for `<…|…>` notation.
- [`siteswap()`](https://ellakaye.github.io/jugglr/reference/Siteswap.md)
  creates a vanilla, synchronous, multiplex, or synchronous multiplex
  siteswap object from a notation string.
- [`synchronousMultiplexSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousMultiplexSiteswap.md)
  creates a synchronous multiplex siteswap S7 object for patterns
  combining simultaneous two-handed throws with multiplex groups, such
  as `"(2,4)([4x4],2x)"`.
- [`synchronousSiteswap()`](https://ellakaye.github.io/jugglr/reference/synchronousSiteswap.md)
  creates a synchronous siteswap S7 object.
- [`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md)
  returns the raw throw data frame underlying the visualisation
  functions, for use in custom visualisations.
- [`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
  now works for `passingSiteswap` objects, drawing a multi-lane arc
  diagram with passes shown as arcs connecting juggler lanes.
- [`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
  now works for `synchronousSiteswap` objects.
- [`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
  plots arc/timeline diagrams for vanilla and multiplex siteswaps.
- [`vanillaSiteswap()`](https://ellakaye.github.io/jugglr/reference/vanillaSiteswap.md)
  creates a vanilla siteswap S7 object.
