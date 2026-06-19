#' Get throw data for a siteswap
#'
#' Returns a data frame describing each throw in the simulated pattern,
#' including which beat and hand each throw originates from, where it lands,
#' and which prop is thrown. This underpins [timeline()] and [ladder()], and
#' can be used directly for custom visualisations.
#'
#' @param siteswap Any siteswap object: a `vanillaSiteswap`,
#'   `synchronousSiteswap`, `multiplexSiteswap`, `synchronousMultiplexSiteswap`,
#'   or `passingSiteswap`.
#' @param n_cycles Number of complete cycles to simulate (default 3). Increase
#'   for patterns with many props to ensure all appear in the data. Setting
#'   `n_cycles >= period * n_props * 2` guarantees each prop is thrown at least
#'   twice.
#' @param ... Additional arguments passed to methods.
#'
#' @returns A data frame with one row per throw and columns:
#'   - `beat`: throw beat index
#'   - `hand`: throwing hand (0 or 1)
#'   - `throw`: throw height (integer)
#'   - `catch_beat`: beat on which the prop lands
#'   - `catch_hand`: hand that catches (0 or 1)
#'   - `prop`: prop identifier (integer)
#'
#'   Synchronous types add an `is_crossing` column. Passing types instead
#'   include `juggler`, `is_pass`, and `catch_juggler` columns identifying which
#'   juggler throws and catches each prop.
#'
#' @examples
#' s <- vanillaSiteswap("531")
#' throw_data(s)
#'
#' @export
throw_data <- new_generic(
  "throw_data",
  "siteswap",
  function(siteswap, n_cycles = 3, ...) S7_dispatch()
)

#' Plot a timeline diagram for a siteswap
#'
#' Produces a timeline (arc) diagram showing the trajectory of each prop across
#' beats. Each arc represents a throw, with height proportional to the throw
#' value and coloured by prop using the colour-blind-friendly Okabe-Ito palette
#' (a ggplot2 default scale is used beyond seven props). The x-axis labels show
#' the throw heights from the siteswap sequence.
#'
#' `timeline()` is an S7 generic. There are methods available for the following
#' classes:
#'
#' `r doclisting::methods_list("timeline")`
#'
#' See **Behaviour by siteswap type** section below for details on the
#' differences between the resulting plots.
#'
#' The result is a standard ggplot2 object, so beyond the `title` and `subtitle`
#' toggles you can modify it by adding any ggplot2 function, for example
#' [ggplot2::labs()] or [ggplot2::scale_colour_manual()].
#'
#' @param siteswap A [vanillaSiteswap],
#'   [synchronousSiteswap], [multiplexSiteswap], [synchronousMultiplexSiteswap],
#'   or [passingSiteswap] object.
#' @param n_cycles Number of complete cycles to simulate (default 3). A warning
#'   is issued if not all props appear within the simulated range.
#' @param title Logical. If `TRUE` (default), adds a title showing the siteswap
#'   sequence. Set to `FALSE` to suppress; override with [ggplot2::labs()] on
#'   the returned plot.
#' @param subtitle Logical. If `TRUE` (default), adds a subtitle showing the
#'   siteswap type and number of props. Set to `FALSE` to suppress; override
#'   with [ggplot2::labs()] on the returned plot.
#' @param ... Additional arguments passed to methods.
#'
#' @returns A ggplot2 object.
#'
#' @section Behaviour by siteswap type:
#'
#' * **Vanilla and multiplex** patterns are drawn single-sided, with all arcs
#'   above a single baseline. The x-axis labels are the throw heights, and
#'   multiplex labels keep their slot brackets (for example `[54]`).
#' * **Synchronous and synchronous multiplex** patterns are drawn two-sided:
#'   one hand's arcs sit above a faint centre line and the other hand's arcs sit
#'   below it. Each hand gets its own x-axis labels, and the subtitle
#'   notes the two-sided layout.
#' * **Multiplex** patterns (synchronous or not) that throw two or more
#'   identical props on the same beat fan those arcs to slightly different peak
#'   heights, so otherwise-overlapping throws render as concentric parabolas.
#' * **Passing** patterns are drawn with one lane per juggler; passes arc
#'   between lanes. Fractional notation offsets the beat positions accordingly.
#'
#' @examples
#' # Vanilla: single-sided arcs
#' timeline(siteswap("423"))
#'
#' # Synchronous: two-sided, hands mirrored about a centre line
#' timeline(siteswap("(4,2x)(2x,4)"))
#' timeline(siteswap("(2,6x)([6x4x],2x)"))
#'
#' # Multiplex: identical simultaneous throws fan to distinct heights
#' timeline(siteswap("[54]24"))
#' timeline(siteswap("[33]"))
#'
#' # Passing: one lane per juggler, with passes arcing between them
#' timeline(siteswap("<3p 3|3p 3>"))
#' timeline(siteswap("<4.5 3 3 | 3 4 3.5>"))
#'
#' # Increasing n_cycles helpful for sequences with short period
#' timeline(siteswap("3"), n_cycles = 6)
#'
#' # ggplot2 object: customise like any other plot
#' timeline(siteswap("423")) + ggplot2::labs(title = "423 or W")
#'
#' @export
timeline <- new_generic(
  "timeline",
  "siteswap",
  function(siteswap, n_cycles = 3, title = TRUE, subtitle = TRUE, ...) {
    S7_dispatch()
  }
)

#' Plot a ladder diagram for a siteswap
#'
#' Produces a ladder diagram showing prop paths between hands across beats. The
#' rails represent the hands and the rungs represent the beats. Each throw
#' connects to its catch: cross-hand throws are drawn as straight segments
#' between the rails, while same-hand throws curve back to the same rail.
#' Throws are coloured by prop using the colour-blind-friendly Okabe-Ito
#' palette (a ggplot2 default scale is used beyond seven props), and beat
#' numbers are labelled along the axis.
#'
#' `ladder()` is an S7 generic. There are methods available for the following
#' classes:
#'
#' `r doclisting::methods_list("ladder")`
#'
#' Where multiplex throws overlap (e.g. `[33]`), the lines fan apart slightly
#' so they are visible. Passing patterns use one pair of rails per juggler,
#' spaced by `hand_gap` and throws between jugglers cross between the rails.
#'
#' The result is a standard ggplot2 object, so beyond the `title` and `subtitle`
#' toggles you can modify it by adding any ggplot2 function, for example
#' [ggplot2::labs()] or [ggplot2::scale_colour_manual()].
#'
#' @param siteswap A [vanillaSiteswap],
#'   [synchronousSiteswap], [multiplexSiteswap], [synchronousMultiplexSiteswap],
#'   or [passingSiteswap] object.
#' @param n_cycles Number of complete cycles to simulate (default 3).
#' @param direction Orientation of the diagram: `"horizontal"` (default, time
#'   runs left to right) or `"vertical"` (time runs top to bottom). Shorthands
#'   `"h"` and `"v"` are also accepted.
#' @param title Logical. If `TRUE` (default), adds a title showing the siteswap
#'   sequence. Set to `FALSE` to suppress; override with [ggplot2::labs()] on
#'   the returned plot.
#' @param subtitle Logical. If `TRUE` (default), adds a subtitle showing the
#'   siteswap type and number of props. Set to `FALSE` to suppress; override
#'   with [ggplot2::labs()] on the returned plot.
#' @param ... Additional arguments passed to methods. For `passingSiteswap`
#'   objects, `hand_gap` (default `2`) sets the vertical spacing between each
#'   juggler's two hands in the diagram.
#'
#' @returns A ggplot2 object.
#'
#' @examples
#' # Vanilla: straight cross-hand throws, curved same-hand throws
#' ladder(siteswap("423"))
#'
#' # Synchronous: even-only beat numbering, crossing denoted by "x"
#' ladder(siteswap("(4,2x)(2x,4)"))
#' ladder(siteswap("(2,6x)([6x4x],2x)"))
#'
#' # Multiplex: identical simultaneous throws fan into separate curves
#' ladder(siteswap("[33]"), n_cycles = 6)
#'
#' # Passing: one pair of rails per juggler
#' ladder(siteswap("<3p 3|3p 3>"))
#' ladder(siteswap("<4.5 3 3 | 3 4 3.5>"))
#'
#' # Vertical orientation, and customise like any ggplot2 object
#' ladder(siteswap("423"), direction = "v", subtitle = FALSE) +
#'   ggplot2::labs(title = "423 or W")
#'
#' @export
ladder <- new_generic(
  "ladder",
  "siteswap",
  function(
    siteswap,
    n_cycles = 3,
    direction = c("horizontal", "vertical", "h", "v"),
    title = TRUE,
    subtitle = TRUE,
    ...
  ) {
    S7_dispatch()
  }
)
