#' @include siteswap.R
#' @include utils.R
#' @include utils-plotting.R
#' @include utils-sync.R
#' @include utils-sync-multiplex.R
#' @include utils-siteswap.R
NULL

#' Synchronous multiplex siteswap
#'
#' Creates a synchronous multiplex siteswap object. This combines synchronous
#' notation (both hands throw simultaneously, written as pairs in parentheses)
#' with multiplex notation (square brackets group simultaneous throws from the
#' same hand). For example, `"(2,4)([4x4],2x)"` is a 4-prop pattern where the
#' second slot has hand 0 throwing two balls simultaneously.
#'
#' All individual throw heights must be even. An `x` suffix marks a crossing
#' throw; a trailing `*` indicates the pattern alternates between two mirrored
#' versions.
#'
#' @param sequence A single character string of synchronous multiplex siteswap
#'   notation, e.g. `"(2,4)([4x4],2x)"`.
#'
#' @returns A `synchronousMultiplexSiteswap` S7 object.
#'
#' @prop type Always `"synchronous multiplex"` (read-only).
#' @prop full_sequence The expanded sequence with the `*` shorthand resolved.
#' @prop throws Character vector of all slot throw strings across one expanded
#'   cycle. Multiplex groups are kept as single elements, e.g. `"[4x4]"`.
#' @prop throws_by_hand Named list with elements `hand_1` and `hand_2`, each a
#'   character vector of throw strings per slot (one per sync slot).
#' @prop period Number of throw slots per full cycle (2 × number of sync slots,
#'   since both hands throw on every beat; always even).
#' @prop symmetry `"symmetrical"` if the pattern is its own mirror image;
#'   `"asymmetrical"` otherwise.
#' @prop n_props Number of props: sum of all throw heights divided by the
#'   period.
#' @prop can_throw `TRUE` if thrown and landing prop counts balance at every
#'   (slot, hand) pair within one cycle.
#' @prop satisfies_average_theorem `TRUE` if `n_props` is a whole number.
#' @prop valid `TRUE` if both `can_throw` and `satisfies_average_theorem` are
#'   `TRUE`.
#'
#' @export
#'
#' @examples
#' synchronousMultiplexSiteswap("(2,4)([4x4],2x)")
#' synchronousMultiplexSiteswap("(4,[42x])*")
#'
#' s <- synchronousMultiplexSiteswap("(2,4)([4x4],2x)")
#' s@n_props
#' s@valid
synchronousMultiplexSiteswap <- new_class(
  "synchronousMultiplexSiteswap",
  properties = list(
    type = new_property(class = class_character, getter = function(self) {
      "synchronous multiplex"
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
        get_sync_multiplex_throws(self@full_sequence)
      }
    ),
    throws_by_hand = new_property(
      class = class_list,
      getter = function(self) {
        get_sync_multiplex_hands(self@full_sequence)
      }
    ),
    period = new_property(
      class = class_integer,
      getter = function(self) {
        length(self@throws_by_hand[["hand_1"]]) * 2L
      }
    ),
    symmetry = new_property(
      class = class_character,
      getter = function(self) {
        ifelse(
          sync_multiplex_symmetrical(self@full_sequence),
          "symmetrical",
          "asymmetrical"
        )
      }
    ),
    n_props = new_property(
      class = class_numeric,
      getter = function(self) {
        sum_sync_multiplex_throws(self@throws_by_hand) / self@period
      }
    ),
    can_throw = new_property(
      class = class_logical,
      getter = function(self) {
        n_sync_slots <- length(self@throws_by_hand[["hand_1"]])
        can_sync_multiplex_throw(self@throws_by_hand, n_sync_slots)
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
    if (!is_sync_multiplex_notation(self@sequence)) {
      cli::cli_abort(
        "@sequence is not valid synchronous multiplex siteswap notation.",
        class = "jugglr_error_invalid_sequence"
      )
    }
    if (!only_even_throws_sync_multiplex(self@sequence)) {
      cli::cli_abort(
        "@sequence must only contain even-value throws.",
        class = "jugglr_error_invalid_sequence"
      )
    }
  },
  parent = Siteswap,
  package = "jugglr"
)

method(print_details, synchronousMultiplexSiteswap) <- function(x, ...) {
  sync_print_details(x)
}

method(throw_data, synchronousMultiplexSiteswap) <- function(
  siteswap,
  n_cycles = 3
) {
  check_n_cycles(n_cycles)
  hands <- siteswap@throws_by_hand
  n_slots <- length(hands$hand_1)
  n_total <- n_slots * n_cycles

  rows <- list()
  row_idx <- 0L

  for (cycle in seq_len(n_cycles)) {
    for (slot_idx in seq_len(n_slots)) {
      beat <- (cycle - 1L) * n_slots + slot_idx

      for (hand in 0:1) {
        slot_throw <- if (hand == 0L) {
          hands$hand_1[[slot_idx]]
        } else {
          hands$hand_2[[slot_idx]]
        }
        parsed <- parse_sync_multiplex_slot(slot_throw)

        for (j in seq_along(parsed$heights)) {
          h <- parsed$heights[[j]]
          cross <- parsed$crossings[[j]]
          catch_beat <- beat + as.integer(h / 2L)
          catch_hand <- if (cross) 1L - hand else hand
          row_idx <- row_idx + 1L
          rows[[row_idx]] <- data.frame(
            beat = beat,
            hand = hand,
            throw = h,
            is_crossing = cross,
            catch_beat = catch_beat,
            catch_hand = catch_hand,
            prop = NA_real_
          )
        }
      }
    }
  }

  throws <- do.call(rbind, rows[seq_len(row_idx)])

  # Prop tracking: multiple props may share (beat, hand), so use first-NA lookup
  assign_props_first_na(throws)
}

method(timeline, synchronousMultiplexSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  title = TRUE,
  subtitle = TRUE
) {
  build_sync_timeline_plot(siteswap, n_cycles, title, subtitle)
}

method(ladder, synchronousMultiplexSiteswap) <- function(
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
