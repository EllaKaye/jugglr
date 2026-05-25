is_passing_notation <- function(sequence) {
  str_detect(sequence, "^<[^>|]+(\\|[^>|]+)+>$")
}

is_fractional_notation <- function(sequence) {
  str_detect(sequence, "\\.")
}

parse_passing_sequence <- function(sequence, fractional = FALSE) {
  inner <- str_sub(sequence, 2L, nchar(sequence) - 1L)
  sections <- str_split(inner, "\\|")[[1L]]
  if (fractional) {
    lapply(sections, \(s) str_extract_all(s, "[0-9]+(\\.5)?")[[1L]])
  } else {
    lapply(sections, \(s) str_extract_all(s, "[0-9a-zA-Z]p?")[[1L]])
  }
}

get_passing_throws <- function(juggler_tokens, fractional = FALSE) {
  if (fractional) {
    as.numeric(juggler_tokens)
  } else {
    chr_throws_to_num(str_remove(juggler_tokens, "(?<=.)p$"))
  }
}

get_pass_flags <- function(juggler_tokens, fractional = FALSE) {
  if (fractional) {
    str_detect(juggler_tokens, "\\.5$")
  } else {
    str_detect(juggler_tokens, "(?<=.)p$")
  }
}

can_pass_throw <- function(
  throws_by_juggler,
  is_pass_by_juggler,
  period,
  fractional = FALSE
) {
  n_jugglers <- length(throws_by_juggler)
  triples <- vector("list", n_jugglers * period)
  k <- 0L

  if (!fractional) {
    for (j in seq_len(n_jugglers)) {
      throws <- throws_by_juggler[[j]]
      is_pass <- is_pass_by_juggler[[j]]
      for (t in seq_len(period)) {
        h <- throws[t]
        catch_beat <- ((t - 1L) + h) %% period + 1L
        catch_juggler <- if (is_pass[t]) (j %% n_jugglers) + 1L else j
        catch_hand <- (t + h - 1L) %% 2L
        k <- k + 1L
        triples[[k]] <- c(catch_juggler, catch_beat, catch_hand)
      }
    }
  } else {
    for (j in seq_len(n_jugglers)) {
      throws <- throws_by_juggler[[j]]
      is_pass <- is_pass_by_juggler[[j]]
      throw_offset <- (j - 1L) * 0.5
      for (t in seq_len(period)) {
        h <- throws[t]
        throw_combined <- throw_offset + (t - 1L)
        catch_raw <- round(throw_combined + h, 10L)
        catch_mod <- catch_raw - period * floor(catch_raw / period)
        frac <- catch_mod %% 1
        catch_juggler <- if (abs(frac) < 1e-9) 1L else 2L
        catch_beat <- as.integer(floor(catch_mod)) + 1L
        catch_hand <- (catch_beat - 1L) %% 2L
        k <- k + 1L
        triples[[k]] <- c(catch_juggler, catch_beat, catch_hand)
      }
    }
  }

  mat <- do.call(rbind, triples)
  anyDuplicated(mat) == 0L
}
