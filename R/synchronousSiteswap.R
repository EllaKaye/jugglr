#' @export
synchronousSiteswap <- new_class(
  "synchronousSiteswap",
  properties = list(
    type = new_property(
      class = class_character,
      setter = function(self, value) {
        if (!is.null(self@type)) {
          stop("@type is read-only", call. = FALSE)
        }
        self@type <- "synchronous"
        self
      }
    ),
    #   throws = new_property(
    #     class = class_integer,
    #     getter = function(self) {
    #       get_throws(self@sequence)
    #     }
    #   ),
    #   period = new_property(
    #     class = class_integer,
    #     getter = function(self) {
    #       length(self@throws)
    #     }
    #   ),
    #   symmetry = new_property(
    #     class = class_character,
    #     getter = function(self) {
    #       ifelse(is_even(self@period), "asymmetrical", "symmetrical")
    #     }
    #   ),
    # TODO: implement properly!
    n_props = new_property(
      class = class_numeric,
      getter = function(self) {
        #mean(self@throws)
        4
      }
    )
    #   can_throw = new_property(
    #     class = class_logical,
    #     getter = function(self) {
    #       can_throw(self@throws)
    #     }
    #   ),
    #   satisfies_average_theorem = new_property(
    #     class = class_logical,
    #     getter = function(self) {
    #       is_whole_number(mean(self@throws))
    #     }
    #   ),
    #   valid = new_property(
    #     class = class_logical,
    #     getter = function(self) {
    #       self@satisfies_average_theorem && self@can_throw
    #     }
    #   )
  ),
  validator = function(self) {
    if (!(str_detect(self@sequence, "^(\\([0-9a-z]x?,[0-9a-z]x?\\))+\\*?$"))) {
      return("@sequence is not valid synchronous siteswap notation")
    }

    # TODO: proper checking for any odd numbers!
    if (str_detect(self@sequence, "1")) {
      return("@sequence must only contain even-value throws")
    }
  },
  parent = Siteswap,
  package = "jugglr"
)
