is_multiplex_notation <- function(sequence) {
  str_detect(sequence, "^([a-zA-Z0-9]|\\[[a-zA-Z0-9]+\\])+$") &&
    str_detect(sequence, "\\[")
}

get_multiplex_slots <- function(sequence) {
  slot_strings <- str_extract_all(
    sequence,
    "\\[[a-zA-Z0-9]+\\]|[a-zA-Z0-9]"
  )[[1]]
  lapply(slot_strings, \(s) get_throws(str_remove_all(s, "[\\[\\]]")))
}

can_multiplex_throw <- function(slots) {
  n <- length(slots)
  throw_counts <- lengths(slots)
  landing_counts <- integer(n)
  for (i in seq_len(n)) {
    for (h in slots[[i]]) {
      land <- ((i - 1L) + h) %% n + 1L
      landing_counts[land] <- landing_counts[land] + 1L
    }
  }
  all(throw_counts == landing_counts)
}
