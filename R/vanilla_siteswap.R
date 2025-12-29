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
    can_throw = S7::new_property(
      class = S7::class_logical,
      getter = function(self) {
        can_throw(self@throws)
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
        self@satisfies_average_theorem && self@can_throw
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

asynchronous_siteswap <- vanilla_siteswap
async_siteswap <- vanilla_siteswap

# TODO: useful print method!
# S7::method(print, vanilla_siteswap) <- function(x, ...) {
#   cat("This is vanilla siteswap")
# }
