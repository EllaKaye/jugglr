#' @export
siteswap <- S7::new_class(
  "siteswap",
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

# TODO: add validators
# Check regex for siteswap
# Can I validate `notation` for vanilla here, given `notation` is defined in parent class?
# TODO: tolower notaion?
#' @export
vanilla_siteswap <- S7::new_class(
  "vanilla_siteswap",
  properties = list(
    throws = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        get_throws(self@notation)
      }
    ),
    period = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        length(self@throws)
      }
    ),
    n_props = S7::new_property(
      class = S7::class_numeric,
      getter = function(self) {
        mean(self@throws)
      }
    ),
    has_collisions = S7::new_property(
      class = S7::class_logical,
      getter = function(self) {
        !no_collisions(self@throws)
      }
    ),
    satisfies_average_theorem = S7::new_property(
      class = S7::class_logical,
      getter = function(self) {
        is_whole_number(mean(self@throws))
      }
    ),
    valid = S7::new_property(
      class = S7::class_logical,
      getter = function(self) {
        self@satisfies_average_theorem && !self@has_collisions
      }
    )
  ),
  validator = function(self) {
    if (!(grepl("^[a-zA-Z0-9]+$", self@notation))) {
      "@notation must only contain digits and letters"
    }
  },
  parent = siteswap,
  package = "jugglr"
)

# TODO: useful print method!
S7::method(print, vanilla_siteswap) <- function(x, ...) {
  cat("This is vanilla siteswap")
}
