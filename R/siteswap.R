#' @include utils-multiplex.R
NULL

#' Siteswap base class
#'
#' Abstract parent class for all siteswap types. Not intended to be
#' instantiated directly; use [siteswap()], [vanillaSiteswap()],
#' [synchronousSiteswap()], or [multiplexSiteswap()] instead.
#'
#' @param sequence A single character string of siteswap notation.
#'
#' @prop sequence The raw siteswap sequence string.
Siteswap <- new_class(
  "Siteswap",
  properties = list(
    sequence = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value) != 1) {
          "must be length 1"
        }
      },
    )
  ),
  package = "jugglr"
)

# TODO: print method for Siteswap (same info for vanilla, sync and multiplex)
# To catch cases where not a recognised type
# or will this be an error in creating the object?

#' Create a siteswap object
#'
#' Creates a typed siteswap object by detecting the notation style of
#' `sequence`. Vanilla siteswaps (alphanumeric strings such as `"531"`) produce
#' a [vanillaSiteswap] object; synchronous siteswaps in `(a,b)` notation such
#' as `"(4,2x)*"` produce a [synchronousSiteswap] object; multiplex siteswaps
#' with square-bracket groups such as `"[43]1"` produce a
#' [multiplexSiteswap] object.
#'
#' @param sequence A single character string of siteswap notation.
#'
#' @returns A [vanillaSiteswap], [synchronousSiteswap], or [multiplexSiteswap]
#'   S7 object.
#'
#' @export
#'
#' @examples
#' siteswap("531")
#' siteswap("(4,2x)*")
#' siteswap("[43]1")
siteswap <- function(sequence) {
  if (str_detect(sequence, "^[a-zA-Z0-9]+$")) {
    vanillaSiteswap(sequence)
  } else if (is_sync_notation(sequence)) {
    synchronousSiteswap(sequence)
  } else if (is_multiplex_notation(sequence)) {
    multiplexSiteswap(sequence)
  } else {
    cli::cli_abort(
      "Not valid vanilla, synchronous, or multiplex siteswap notation.",
      class = "jugglr_error_not_valid_siteswap"
    )
  }
}
