#' @include siteswap.R
#' @include utils.R
#' @include utils-plotting.R
#' @include utils-multiplex.R
#' @include utils-siteswap.R
NULL

#' Multiplex siteswap
#'
#' Creates a multiplex siteswap object from a sequence in which square brackets
#' group simultaneous throws from the same hand. For example, `"[43]1"` means
#' the juggler throws heights 4 and 3 simultaneously on the first beat, then a
#' single 1 on the second beat. Any slot without brackets is a single throw
#' exactly as in vanilla siteswap.
#'
#' @param sequence A single character string of multiplex siteswap notation,
#'   e.g. `"[43]1"` or `"[33]"`.
#'
#' @returns A `multiplexSiteswap` S7 object.
#'
#' @prop type Always `"multiplex"` (read-only).
#' @prop slots List of integer vectors, one element per time slot. Single-throw
#'   slots are length-1 vectors; multiplex slots have length > 1.
#' @prop throws Integer vector of all individual throw heights (flattened across
#'   all slots).
#' @prop period Number of time slots per cycle.
#' @prop symmetry `"symmetrical"` when period is odd; `"asymmetrical"` when
#'   period is even.
#' @prop n_props Total of all throw heights divided by the period — the number
#'   of props required. (Note: `sum(throws) / period`, not `mean(throws)`.)
#' @prop can_throw `TRUE` if for every slot the number of props thrown equals
#'   the number of props landing (no conservation violation).
#' @prop satisfies_average_theorem `TRUE` if `n_props` is a whole number.
#' @prop valid `TRUE` if both `can_throw` and `satisfies_average_theorem` are
#'   `TRUE`.
#'
#' @export
#'
#' @examples
#' multiplexSiteswap("[43]1")
#' multiplexSiteswap("[33]")
#'
#' s <- multiplexSiteswap("[43]1")
#' s@n_props
#' s@valid
multiplexSiteswap <- new_class(
  "multiplexSiteswap",
  properties = list(
    type = new_property(class = class_character, getter = function(self) {
      "multiplex"
    }),
    slots = new_property(
      class = class_list,
      getter = function(self) {
        get_multiplex_slots(self@sequence)
      }
    ),
    throws = new_property(
      class = class_integer,
      getter = function(self) {
        unlist(self@slots)
      }
    ),
    period = new_property(
      class = class_integer,
      getter = function(self) {
        length(self@slots)
      }
    ),
    symmetry = new_property(
      class = class_character,
      getter = function(self) {
        ifelse(is_even(self@period), "asymmetrical", "symmetrical")
      }
    ),
    n_props = new_property(
      class = class_numeric,
      getter = function(self) {
        sum(self@throws) / self@period
      }
    ),
    can_throw = new_property(
      class = class_logical,
      getter = function(self) {
        can_multiplex_throw(self@slots)
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
    if (!is_multiplex_notation(self@sequence)) {
      cli::cli_abort(
        "@sequence is not valid multiplex siteswap notation.",
        class = "jugglr_error_invalid_sequence"
      )
    }
  },
  parent = Siteswap,
  package = "jugglr"
)


method(throw_data, multiplexSiteswap) <- function(siteswap, n_cycles = 3) {
  check_n_cycles(n_cycles)
  period <- siteswap@period
  n_slots_total <- period * n_cycles

  rows <- vector("list", n_slots_total * 2L)
  row_idx <- 0L

  for (cycle in seq_len(n_cycles)) {
    for (slot_idx in seq_len(period)) {
      slot_offset <- (cycle - 1L) * period + slot_idx
      hand <- (slot_offset - 1L) %% 2L
      slot_throws <- siteswap@slots[[slot_idx]]

      for (h in slot_throws) {
        row_idx <- row_idx + 1L
        catch_hand <- ifelse(is_even(h), hand, 1L - hand)
        rows[[row_idx]] <- data.frame(
          beat = slot_offset,
          hand = hand,
          throw = h,
          catch_beat = slot_offset + h,
          catch_hand = catch_hand,
          prop = NA_real_
        )
      }
    }
  }

  throws <- do.call(rbind, rows[seq_len(row_idx)])

  # Prop tracking: multiple props may share (beat, hand), so use first-NA lookup
  assign_props_first_na(throws)
}

method(timeline, multiplexSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  title = TRUE,
  subtitle = TRUE
) {
  slot_labels <- vapply(
    siteswap@slots,
    function(s) {
      if (length(s) == 1L) {
        as.character(s)
      } else {
        paste0("[", paste(s, collapse = ""), "]")
      }
    },
    character(1L)
  )

  build_simple_timeline(
    siteswap,
    n_cycles,
    title,
    subtitle,
    x_labels = slot_labels,
    title_seq = siteswap@sequence
  )
}

method(ladder, multiplexSiteswap) <- function(
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
    is_even(throw),
    siteswap@sequence
  )
}
