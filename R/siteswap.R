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
    throws = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        get_throws(self@notation)
      }
    ),
    # TODO:
    # properties for has_collisions and satisfies_average_theorm
    # then compute `valid` from those
    valid = S7::new_property(
      class = S7::class_logical,
      getter = function(self) {
        # TODO: actual test for validity!
        is_whole_number(mean(self@throws))
      }
    )
  ),
  parent = siteswap,
  package = "jugglr"
)
