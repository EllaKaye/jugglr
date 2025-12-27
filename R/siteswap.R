#' @export
siteswap <- S7::new_class(
  "siteswap",
  properties = list(
    notation = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        self@notation <- as.character(value)
        self
      }
    )
  ),
  package = "jugglr"
)
