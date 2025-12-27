#' @export
siteswap <- S7::new_class(
  "siteswap",
  properties = list(
    notation = S7::class_character
  ),
  package = "jugglr"
)

vanilla_siteswap <- S7::new_class(
  "vanilla_siteswap",
  properties = list(
    valid = S7::new_property(
      class = S7::class_logical,
      getter = function(self) {
        # TODO: actual test for validity!
        is_whole_number(as.numeric(self@notation))
      }
    )
  ),
  parent = siteswap,
  package = "jugglr"
)
