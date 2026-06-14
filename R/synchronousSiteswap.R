#' @include siteswap.R
#' @include utils.R
#' @include utils-plotting.R
#' @include utils-sync.R
NULL

#' Synchronous siteswap
#'
#' Creates a synchronous siteswap object from a two-handed simultaneous-throw
#' sequence. Synchronous notation describes patterns where both hands throw at
#' the same time, written as pairs of throw heights in parentheses such as
#' `"(4,4)"` or `"(4,2x)*"`. An `x` suffix marks a crossing throw; a trailing
#' `*` indicates the pattern alternates between two mirrored versions.
#'
#' @param sequence A single character string of synchronous siteswap notation,
#'   e.g. `"(4,4)"` or `"(4,2x)*"`.
#'
#' @returns A `synchronousSiteswap` S7 object.
#'
#' @prop type Always `"synchronous"` (read-only).
#' @prop full_sequence The expanded sequence with the `*` shorthand resolved.
#' @prop throws Character vector of all individual throw values across one
#'   expanded cycle.
#' @prop throws_by_hand Named list with elements `hand_1` and `hand_2`,
#'   each a character vector of throws for that hand per slot.
#' @prop period Number of throw slots per full cycle (counts both hands per
#'   simultaneous beat, so always even).
#' @prop symmetry `"symmetrical"` if the pattern is its own mirror image;
#'   `"asymmetrical"` otherwise.
#' @prop n_props Mean of the slide sequence, equal to the number of props.
#' @prop can_throw `TRUE` if no collisions occur in the slide sequence.
#' @prop satisfies_average_theorem `TRUE` if `n_props` is a whole number.
#' @prop valid `TRUE` if both `can_throw` and `satisfies_average_theorem` are
#'   `TRUE`.
#'
#' @export
#'
#' @examples
#' synchronousSiteswap("(4,4)")
#' synchronousSiteswap("(4,2x)*")
#'
#' s <- synchronousSiteswap("(4,2x)*")
#' s@n_props
#' s@valid
synchronousSiteswap <- new_class(
  "synchronousSiteswap",
  properties = list(
    type = new_property(class = class_character, getter = function(self) {
      "synchronous"
    }),
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
      class = class_list,
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
    if (!is_sync_notation(self@sequence)) {
      cli::cli_abort(
        "@sequence is not valid synchronous siteswap notation.",
        class = "jugglr_error_invalid_sequence"
      )
    }
    if (!only_even_throws(self@sequence)) {
      cli::cli_abort(
        "@sequence must only contain even-value throws.",
        class = "jugglr_error_invalid_sequence"
      )
    }
  },
  parent = Siteswap,
  package = "jugglr"
)

method(print_details, synchronousSiteswap) <- function(x, ...) {
  sync_print_details(x)
}

method(throw_data, synchronousSiteswap) <- function(siteswap, n_cycles = 3) {
  check_n_cycles(n_cycles)
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
  throw_val <- chr_sync_throws_to_num(throw_raw)
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

method(timeline, synchronousSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  title = TRUE,
  subtitle = TRUE
) {
  build_simple_timeline(
    siteswap,
    n_cycles,
    title,
    subtitle,
    x_labels = sync_slot_labels(siteswap@full_sequence),
    title_seq = siteswap@full_sequence,
    by_hand = TRUE,
    subtitle_extra = "Solid and dashed lines represent different hands."
  )
}

method(ladder, synchronousSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  direction = c("horizontal", "vertical", "h", "v"),
  title = TRUE,
  subtitle = TRUE
) {
  direction <- normalise_direction(direction)
  build_simple_ladder(
    siteswap,
    n_cycles,
    direction,
    title,
    subtitle,
    !is_crossing,
    siteswap@full_sequence
  )
}
