#' @export
Siteswap <- S7::new_class(
  "Siteswap",
  properties = list(
    notation = S7::new_property(
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

# TODO: other flavours of siteswap
siteswap <- function(notation) {
  if (str_detect(notation, "^[a-wA-W0-9]+$")) {
    vanillaSiteswap(notation)
  } else {
    Siteswap(notation)
  }
}
