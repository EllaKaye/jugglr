is_sync_multiplex_notation <- function(sequence) {
  slot_pat <- "(?:[0-9a-z]x?|\\[(?:[0-9a-z]x?)+\\])"
  pair_pat <- paste0("\\(", slot_pat, ",", slot_pat, "\\)")
  full_pat <- paste0("^(", pair_pat, ")+\\*?$")
  str_detect(sequence, full_pat) && str_detect(sequence, "\\[")
}

# Returns character vector preserving "[...]" multiplex groups as single elements
# e.g. "(2,4)([4x4],2x)" -> c("2", "4", "[4x4]", "2x")
get_sync_multiplex_throws <- function(sequence) {
  str_extract_all(sequence, "\\[(?:[0-9a-z]x?)+\\]|[0-9a-z]x?")[[1]]
}

get_sync_multiplex_hands <- function(sequence) {
  throws <- get_sync_multiplex_throws(sequence)
  list(hand_1 = throws[c(TRUE, FALSE)], hand_2 = throws[c(FALSE, TRUE)])
}

# Parse one slot string into numeric heights and crossing flags.
# "[4x4]" -> list(heights = c(4L, 4L), crossings = c(TRUE, FALSE))
# "2x"   -> list(heights = 2L, crossings = TRUE)
parse_sync_multiplex_slot <- function(slot_str) {
  if (str_detect(slot_str, "^\\[")) {
    inner <- str_remove_all(slot_str, "[\\[\\]]")
    individual <- str_extract_all(inner, "[0-9a-z]x?")[[1]]
  } else {
    individual <- slot_str
  }
  list(
    heights = chr_sync_throws_to_num(individual),
    crossings = str_detect(individual, "x$")
  )
}

# Validity check: thrown and landing prop counts must balance at every (slot, hand).
# This is the multiplex equivalent of can_throw() for synchronous patterns.
can_sync_multiplex_throw <- function(hands, n_sync_slots) {
  throw_counts <- matrix(0L, nrow = n_sync_slots, ncol = 2L)
  landing_counts <- matrix(0L, nrow = n_sync_slots, ncol = 2L)

  for (hand in 0:1) {
    slot_throws <- if (hand == 0L) hands$hand_1 else hands$hand_2
    for (slot_idx in seq_len(n_sync_slots)) {
      parsed <- parse_sync_multiplex_slot(slot_throws[[slot_idx]])
      n_t <- length(parsed$heights)
      throw_counts[slot_idx, hand + 1L] <- n_t
      for (j in seq_len(n_t)) {
        h <- parsed$heights[[j]]
        cross <- parsed$crossings[[j]]
        catch_slot <- (slot_idx - 1L + as.integer(h / 2L)) %% n_sync_slots + 1L
        catch_hand <- if (cross) 1L - hand else hand
        landing_counts[catch_slot, catch_hand + 1L] <-
          landing_counts[catch_slot, catch_hand + 1L] + 1L
      }
    }
  }

  all(throw_counts == landing_counts)
}

# All individual throw heights (including inside multiplex groups) must be even.
only_even_throws_sync_multiplex <- function(sequence) {
  slots <- get_sync_multiplex_throws(sequence)
  all_individual <- unlist(lapply(slots, function(s) {
    if (str_detect(s, "^\\[")) {
      inner <- str_remove_all(s, "[\\[\\]]")
      str_extract_all(inner, "[0-9a-z]x?")[[1]]
    } else {
      s
    }
  }))
  all(is_even(chr_sync_throws_to_num(all_individual)))
}

# Sum of all throw heights across both hands (used for n_props).
sum_sync_multiplex_throws <- function(throws_by_hand) {
  all_slots <- c(throws_by_hand$hand_1, throws_by_hand$hand_2)
  sum(unlist(lapply(all_slots, function(s) {
    parse_sync_multiplex_slot(s)$heights
  })))
}

sync_multiplex_symmetrical <- function(sequence) {
  throws <- get_sync_multiplex_throws(sequence)
  left <- throws[c(TRUE, FALSE)]
  right <- throws[c(FALSE, TRUE)]
  rotations_match(left, right)
}
