#' Siteswap base class
#'
#' Abstract parent class for all siteswap types. Not intended to be
#' instantiated directly; use [siteswap()], [vanillaSiteswap()], or
#' [synchronousSiteswap()] instead.
#'
#' @param sequence A single character string of siteswap notation.
#'
#' @prop sequence The raw siteswap sequence string.
Siteswap <- new_class(
  "Siteswap",
  properties = list(
    # TODO: probably need to validate here that sequence is valid siteswap
    # Use combo of different helpers, is_vanilla_siteswap, is_sync_siteswap etc
    sequence = new_property(
      class = class_character,
      validator = function(value) {
        if (length(value) != 1) {
          "must be length 1"
        }
      },
    ) #,
    # ? Do I want `n_props` in Siteswap or just in the subclasses?
    # n_props = new_property(
    #   class = class_numeric,
    #   getter = function(self) {
    #     # TODO: do I want/need something different here?
    #     # Default implementation (could also just error here)
    #     # NOTE: shouldn't need this is sequence is validated as siteswap
    #     # but good to keep as backstop
    #     stop("n_props must be implemented by subclass", call. = FALSE)
    #   }
    # )
  ),
  package = "jugglr"
)

# TODO: print method for Siteswap (same info for vanilla, sync and multiplex)
# To catch cases where not a recognised type
# or will this be an error in creating the object?

# TODO: other flavours of siteswap
#' Create a siteswap object
#'
#' Creates a typed siteswap object by detecting the notation style of
#' `sequence`. Vanilla siteswaps (alphanumeric strings such as `"531"`) produce
#' a [vanillaSiteswap] object; synchronous siteswaps in `(a,b)` notation such
#' as `"(4,2x)*"` produce a [synchronousSiteswap] object.
#'
#' @param sequence A single character string of siteswap notation.
#'
#' @returns A [vanillaSiteswap] or [synchronousSiteswap] S7 object.
#'
#' @export
#'
#' @examples
#' siteswap("531")
#' siteswap("(4,2x)*")
siteswap <- function(sequence) {
  if (str_detect(sequence, "^[a-zA-Z0-9]+$")) {
    vanillaSiteswap(sequence)
  } else if (is_sync_notation(sequence)) {
    synchronousSiteswap(sequence)
  } else {
    # Siteswap(sequence)
    # TODO: better message
    cli::cli_abort(
      "Not valid vanilla or synchronous siteswap notation",
      class = "not_valid_siteswap"
    )
  }
}
