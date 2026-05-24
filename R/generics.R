#' Get throw data for a siteswap
#'
#' Returns a data frame describing each throw in the simulated pattern,
#' including which beat and hand each throw originates from, where it lands,
#' and which prop is thrown. This underpins [timeline()] and [ladder()], and
#' can be used directly for custom visualisations.
#'
#' @param siteswap A `vanillaSiteswap` or `synchronousSiteswap` object.
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
#' @export
throw_data <- new_generic("throw_data", "siteswap")

#' Plot a timeline diagram for a siteswap
#'
#' Produces a timeline (arc) diagram showing the trajectory of each prop
#' across beats. Each arc represents a throw, coloured by prop. The x-axis
#' labels show the throw heights from the siteswap sequence.
#'
#' @param siteswap A `vanillaSiteswap` or `synchronousSiteswap` object.
#' @param n_cycles Number of complete cycles to simulate (default 3). A warning
#'   is issued if not all props appear within the simulated range.
#' @param title Logical. If `TRUE` (default), adds a title and subtitle showing
#'   the sequence and number of props. Set to `FALSE` to suppress; override
#'   with [ggplot2::labs()] on the returned plot.
#' @param ... Additional arguments passed to methods.
#'
#' @returns A ggplot2 object.
#'
#' @export
timeline <- new_generic("timeline", "siteswap")

#' Plot a ladder diagram for a siteswap
#'
#' Produces a ladder diagram showing prop paths between hands across beats.
#' Straight lines indicate self-throws (same hand catches); crossing lines
#' indicate cross-throws (opposite hand catches).
#'
#' @param siteswap A `vanillaSiteswap` or `synchronousSiteswap` object.
#' @param n_cycles Number of complete cycles to simulate (default 3).
#' @param direction Orientation of the diagram: `"horizontal"` (default, time
#'   runs left to right) or `"vertical"` (time runs top to bottom).
#' @param ... Additional arguments passed to methods.
#'
#' @returns A ggplot2 object.
#'
#' @export
ladder <- new_generic("ladder", "siteswap")
