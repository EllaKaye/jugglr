#' @export
synchronousSiteswap <- new_class(
  "synchronousSiteswap",
  properties = list(
    # MAYBE: don't need type if not using in any methods
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
    full_sequence = new_property(
      class = class_character,
      getter = function(self) {
        expand_siteswap(self@sequence)
      }
    ),
    # pairs = new_property(
    #   class = class_character,
    #   getter = function(self) {
    #     get_sync_pairs(self@full_sequence)
    #   }
    # ),
    throws = new_property(
      class = class_character,
      getter = function(self) {
        get_sync_throws(self@full_sequence)
      }
    ),
    throws_by_hand = new_property(
      class = class_character,
      getter = function(self) {
        get_sync_hands(self@full_sequence)
      }
    ),
    period = new_property(
      class = class_integer,
      getter = function(self) {
        length(self@throws["hand_1"]) * 2
      }
    ),
    symmetry = new_property(
      class = class_character,
      getter = function(self) {
        ifelse(
          sync_symmetrical(self@full_sequence),
          "symmetrical",
          "asymmetrical"
        )
      }
    ),
    slide = new_property(
      clas = class_numeric,
      getter = function(self) {
        self@full_sequence |>
          get_sync_throws() |>
          slide()
      }
    ),
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
