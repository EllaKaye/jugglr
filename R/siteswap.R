#' @export
Siteswap <- S7::new_class(
  "Siteswap",
  properties = list(
    sequence = S7::new_property(
      class = S7::class_character,
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

# TODO: other flavours of siteswap
#' @export
siteswap <- function(sequence) {
  if (str_detect(sequence, "^[a-wA-W0-9]+$")) {
    vanillaSiteswap(sequence)
  } else {
    Siteswap(sequence)
  }
}
