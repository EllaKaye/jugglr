# jugglr 0.0.0.9000

* `ladder()` for `passingSiteswap` now uses the sequence as the plot title directly, consistent with all other siteswap types.
* `synchronousSiteswap` `throws_by_hand` property declaration corrected from `class_character` to `class_list` to match the list returned by `get_sync_hands()`.

* `ladder()` and `timeline()` titles now show the siteswap sequence directly (e.g. `531`) rather than prefixing with `"Siteswap '"`. For `synchronousSiteswap` and `synchronousMultiplexSiteswap`, `timeline()` now uses `full_sequence` (e.g. `(4,2x)(4,2x)`) to match the existing `ladder()` behaviour.
* `ladder()` subtitle-to-plot spacing is now direction-aware: horizontal and vertical ladder diagrams use different margins to reduce excess whitespace and prevent subtitle overlap.
* `ladder()` `direction` argument now also accepts `"h"` and `"v"` as shorthands for `"horizontal"` and `"vertical"`.
* `ladder()` gains a `title` argument (default `TRUE`) to control whether the plot title is shown, matching the existing `subtitle` argument.
* `timeline()` gains a `subtitle` argument (default `TRUE`) to control whether the plot subtitle is shown independently of the `title` argument.
* `timeline()` for `passingSiteswap` now shows only J1's throws as x-axis labels, with J2 (and further jugglers') throws displayed as in-plot text annotations below each juggler's lane.
* `timeline()` for `synchronousSiteswap` and `synchronousMultiplexSiteswap` now displays L/R hand throws on separate lines in the axis labels, reducing horizontal crowding.
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
