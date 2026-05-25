# jugglr 0.0.0.9000

* `animate()` and `animate_markdown()` animate juggling patterns via the JugglingLab GIF server.
* `animate()` gains a `prop` argument for selecting the prop type (`"ball"`, `"ring"`, or `"image"`).
* `animate()` and `animate_markdown()` now accept [vanillaSiteswap], [synchronousSiteswap], and [multiplexSiteswap] objects as `pattern`, in addition to plain strings.
* `animate_markdown()` is now exported.
* `ladder()` now works for `passingSiteswap` objects, drawing a multi-row diagram with passes shown as diagonal segments between juggler rows.
* `ladder()` plots ladder diagrams for vanilla, synchronous, and multiplex siteswaps.
* `multiplexSiteswap()` creates a multiplex siteswap S7 object for patterns with simultaneous throws from one hand, written using square-bracket notation such as `"[43]1"`.
* `passingSiteswap()` creates a passing siteswap S7 object for multi-juggler patterns in `<A|B>` notation. Supports both p-notation (e.g. `"<4p 3 | 3 4p>"`) and fractional notation (e.g. `"<4.5 3 3 | 3 4 3.5>"`). Includes `throw_data()`, `timeline()`, and `ladder()` methods.
* `siteswap()` now also dispatches to `passingSiteswap()` for `<…|…>` notation.
* `siteswap()` creates a vanilla, synchronous, multiplex, or synchronous multiplex siteswap object from a notation string.
* `synchronousMultiplexSiteswap()` creates a synchronous multiplex siteswap S7 object for patterns combining simultaneous two-handed throws with multiplex groups, such as `"(2,4)([4x4],2x)"`.
* `synchronousSiteswap()` creates a synchronous siteswap S7 object.
* `throw_data()` returns the raw throw data frame underlying the visualisation functions, for use in custom visualisations.
* `timeline()` now works for `passingSiteswap` objects, drawing a multi-lane arc diagram with passes shown as arcs connecting juggler lanes.
* `timeline()` now works for `synchronousSiteswap` objects.
* `timeline()` plots arc/timeline diagrams for vanilla and multiplex siteswaps.
* `vanillaSiteswap()` creates a vanilla siteswap S7 object.
