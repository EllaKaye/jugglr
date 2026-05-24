#' @include siteswap.R
#' @include utils.R
#' @include utils-plotting.R
#' @include utils-sync.R
NULL

#' @export
synchronousSiteswap <- new_class(
  "synchronousSiteswap",
  properties = list(
    # MAYBE: don't need type if not using in any methods
    type = new_property(
      class = class_character,
      setter = function(self, value) {
        if (!is.null(self@type)) {
          cli::cli_abort("@type is read-only")
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
        length(self@throws_by_hand[["hand_1"]]) * 2
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
      class = class_numeric,
      getter = function(self) {
        self@full_sequence |>
          get_sync_throws() |>
          slide()
      }
    ),
    n_props = new_property(
      class = class_numeric,
      getter = function(self) {
        mean(self@slide[["slide1"]])
      }
    ),
    # sync siteswap is valid if vanilla siteswap is valid
    # for corresponding slide sequence
    can_throw = new_property(
      class = class_logical,
      getter = function(self) {
        can_throw(self@slide[[1]])
      }
    ),
    satisfies_average_theorem = new_property(
      class = class_logical,
      getter = function(self) {
        is_whole_number(self@n_props)
      }
    ),
    valid = new_property(
      class = class_logical,
      getter = function(self) {
        self@satisfies_average_theorem && self@can_throw
      }
    )
  ),
  validator = function(self) {
    if (!(str_detect(self@sequence, "^(\\([0-9a-z]x?,[0-9a-z]x?\\))+\\*?$"))) {
      return("@sequence is not valid synchronous siteswap notation")
    }

    if (!only_even_throws(self@sequence)) {
      return("@sequence must only contain even-value throws")
    }
  },
  parent = Siteswap,
  package = "jugglr"
)

method(throw_data, synchronousSiteswap) <- function(siteswap, n_cycles = 3) {
  hands <- siteswap@throws_by_hand
  n_slots <- length(hands$hand_1)
  n_total <- n_slots * n_cycles

  hand_0_throws <- rep(hands$hand_1, times = n_cycles)
  hand_1_throws <- rep(hands$hand_2, times = n_cycles)

  beat <- rep(seq_len(n_total), each = 2)
  hand <- rep(c(0L, 1L), times = n_total)
  throw_raw <- character(n_total * 2)
  throw_raw[c(TRUE, FALSE)] <- hand_0_throws
  throw_raw[c(FALSE, TRUE)] <- hand_1_throws

  is_crossing <- str_detect(throw_raw, "(?<=.)x$")
  throw_val <- chr_throws_to_num(str_remove(throw_raw, "(?<=.)x$"))
  catch_beat <- beat + as.integer(throw_val / 2)
  catch_hand <- ifelse(is_crossing, 1L - hand, hand)

  throws <- data.frame(
    beat,
    hand,
    throw = throw_val,
    is_crossing,
    catch_beat,
    catch_hand,
    prop = NA_real_
  )

  # Prop tracking: uses (beat, hand) lookup because two rows share each slot.
  next_prop <- 1L
  for (i in seq_len(nrow(throws))) {
    if (throws$throw[i] == 0) {
      next
    }
    current_prop <- throws$prop[i]
    if (is.na(current_prop)) {
      throws$prop[i] <- next_prop
      current_prop <- next_prop
      next_prop <- next_prop + 1L
    }
    idx <- which(
      throws$beat == throws$catch_beat[i] & throws$hand == throws$catch_hand[i]
    )
    if (length(idx) > 0) throws$prop[idx] <- current_prop
  }

  throws
}

method(ladder, synchronousSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  direction = c("horizontal", "vertical")
) {
  direction <- rlang::arg_match(direction)

  plot_data <- throw_data(siteswap, n_cycles = n_cycles) |>
    mutate(is_even = !is_crossing) |>
    filter(throw > 0)

  build_ladder_plot(
    plot_data,
    direction,
    paste("Ladder Diagram: Siteswap", siteswap@full_sequence)
  )
}
