# ? Do I actually want to export this?
# Could get confusing compared to desired behaviour of user calling `siteswap`,
# which assigns subclass, and subclass gives `Siteswap` as parent
# Are there any circumastances in which the user might want `Siteswap` directly?
#' @export
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
# TODO: error if not a valid siteswap string
#' @export
siteswap <- function(sequence) {
  if (str_detect(sequence, "^[a-zA-Z0-9]+$")) {
    vanillaSiteswap(sequence)
    # MAYBE: I want to check that all even here, or in synchronousSiteswap itself?
  } else if (str_detect(sequence, "^(\\([0-9a-z]x?,[0-9a-z]x?\\))+\\*?$")) {
    synchronousSiteswap(sequence)
  } else {
    # Siteswap(sequence)
    # TODO: better message, and give error a class for testing
    cli::cli_abort("Not valid siteswap notation")
  }
}
