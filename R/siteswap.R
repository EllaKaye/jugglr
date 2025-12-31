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

# just testing - n is not a meaningful argument
siteswap <- function(notation, n) {
  if (n == 1) {
    vanillaSiteswap(notation)
  } else {
    Siteswap(notation)
  }
}
