assign_props_first_na <- function(throws) {
  throws$prop[1L] <- 1L
  next_prop <- 2L

  for (i in seq_len(nrow(throws))) {
    if (throws$throw[i] == 0L) {
      next
    }
    current_prop <- throws$prop[i]
    if (is.na(current_prop)) {
      throws$prop[i] <- next_prop
      current_prop <- next_prop
      next_prop <- next_prop + 1L
    }
    idx <- which(
      throws$beat == throws$catch_beat[i] &
        throws$hand == throws$catch_hand[i] &
        is.na(throws$prop)
    )
    if (length(idx) > 0L) throws$prop[idx[1L]] <- current_prop
  }

  throws
}
