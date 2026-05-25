#' @include utils-multiplex.R
#' @include utils-sync-multiplex.R
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

method(print, Siteswap) <- function(x, ...) {
  if (x@valid) {
    cli::cli_bullets(c(
      "v" = "'{x@sequence}' is valid {x@type} siteswap",
      "i" = "It uses {x@n_props} props",
      "i" = "It is {x@symmetry} with period {x@period}"
    ))
  } else {
    reasons <- c(
      if (!x@satisfies_average_theorem) {
        c("i" = "The throws don't average to a whole number")
      },
      if (!x@can_throw) {
        c("i" = "Two or more throws land on the same beat (collision)")
      }
    )
    cli::cli_bullets(c(
      "x" = "'{x@sequence}' is not a valid juggling pattern",
      reasons
    ))
  }
}

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
#' @returns A [vanillaSiteswap], [synchronousSiteswap], [multiplexSiteswap], or
#'   [synchronousMultiplexSiteswap] S7 object.
#'
#' @export
#'
#' @examples
#' siteswap("531")
#' siteswap("(4,2x)*")
#' siteswap("[43]1")
#' siteswap("(2,4)([4x4],2x)")
siteswap <- function(sequence) {
  if (str_detect(sequence, "^[a-zA-Z0-9]+$")) {
    vanillaSiteswap(sequence)
  } else if (is_sync_multiplex_notation(sequence)) {
    synchronousMultiplexSiteswap(sequence)
  } else if (is_sync_notation(sequence)) {
    synchronousSiteswap(sequence)
  } else if (is_multiplex_notation(sequence)) {
    multiplexSiteswap(sequence)
  } else {
    cli::cli_abort(
      "Not valid vanilla, synchronous, multiplex, or synchronous multiplex siteswap notation.",
      class = "jugglr_error_not_valid_siteswap"
    )
  }
}
