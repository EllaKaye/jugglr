#' @export
vanillaSiteswap <- S7::new_class(
  "vanillaSiteswap",
  properties = list(
    type = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        if (!is.null(self@type)) {
          stop("@type is read-only", call. = FALSE)
        }
        self@type <- "vanilla"
        self
      }
    ),
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
    symmetry = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        ifelse(is_even(self@period), "asymmetrical", "symmetrical")
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
    if (!(str_detect(self@notation, "^[a-wA-W0-9]+$"))) {
      "@notation must only contain digits and letters a-w"
    }
  },
  parent = Siteswap,
  package = "jugglr"
)

asynchronousSiteswap <- vanillaSiteswap
asyncSiteswap <- vanillaSiteswap

# TODO: Any other information to print about the pattern?
S7::method(print, vanillaSiteswap) <- function(x, ...) {
  if (x@valid) {
    cli::cli_bullets(
      c(
        "v" = "{x@notation} is valid {x@type} siteswap",
        "i" = "It uses {x@n_props} props",
        "i" = "It is {x@symmetry} with period {x@period}"
      )
    )
  } else {
    cli::cli_bullets(
      c(
        "x" = "This siteswap is not a valid juggling pattern"
      )
    )
  }
}
