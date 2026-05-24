is_sync_notation <- function(sequence) {
  str_detect(sequence, "^(\\([0-9a-z]x?,[0-9a-z]x?\\))+\\*?$")
}

# Turns synchronous notation with `*` into its full version
expand_siteswap <- function(pattern) {
  if (!str_detect(pattern, "\\*$")) {
    return(pattern)
  }

  base_pattern <- str_remove(pattern, "\\*$")
  throws <- str_extract_all(base_pattern, "\\([^)]+\\)")[[1]] # sync pairs

  if (length(throws) == 0) {
    return(pattern)
  }

  # Create mirror version by reversing the sequence and swapping within each group
  mirror_throws <- sapply(throws, function(throw) {
    content <- str_remove_all(throw, "[()]")
    parts <- str_split(content, ",")[[1]]
    paste0("(", paste(rev(parts), collapse = ","), ")")
  })

  # Combine original and mirror
  paste0(base_pattern, paste(mirror_throws, collapse = ""))
}

# throws is result of get_sync_throws(sequence)
# TODO: test with valid (4,2x)(2x,4), (4,6x)(2x,4), (8x,4x)(4,4)
# TODO: and not-valid (6,2)(4x,6x), (6,2)(4,6) sync patterns
# TODO: and test with letter throws
slide <- function(throws) {
  n <- length(throws)
  throws_no_x <- str_remove(throws, "(?<=.)x$")
  throws_num <- chr_throws_to_num(throws_no_x)
  slide1 <- numeric(n)
  slide2 <- numeric(n)

  for (i in seq_len(n)) {
    is_crossing <- stringr::str_detect(throws[i], "(?<=.)x$")
    if (is_crossing) {
      if (is_even(i)) {
        slide1[i] <- throws_num[i] - 1
        slide2[i] <- throws_num[i - 1] - 1
      } else {
        slide1[i] <- throws_num[i] + 1
        slide2[i] <- throws_num[i + 1] + 1
      }
    } else {
      slide1[i] <- throws_num[i]
      slide2[i] <- ifelse(is_even(i), throws_num[i - 1], throws_num[i + 1])
    }
  }

  list(slide1 = slide1, slide2 = slide2)
}

# sequence is (full) sync siteswap sequence
get_sync_throws <- function(sequence) {
  str_extract_all(sequence, "[0-9a-z]x?")[[1]]
}

get_sync_hands <- function(sequence) {
  throws <- get_sync_throws(sequence)
  list(hand_1 = throws[c(TRUE, FALSE)], hand_2 = throws[c(FALSE, TRUE)])
}

# TODO: update to use get_sync_throws
# expects a sequence that is valid sync siteswap notation
# bar possibly having odd throws
only_even_throws <- function(sequence) {
  throws_chr <- get_sync_throws(sequence)

  # Remove trailing 'x' only if preceded by something (it's a crossing marker)
  # Keeps standalone 'x' (it's a throw value of 33)
  throws_chr_no_x <- str_remove(throws_chr, "(?<=.)x$")

  throws <- chr_throws_to_num(throws_chr_no_x)
  all(is_even(throws))
}

sync_symmetrical <- function(sequence) {
  throws <- get_sync_throws(sequence)

  left <- throws[c(TRUE, FALSE)]
  right <- throws[c(FALSE, TRUE)]

  n <- length(right)

  if (n == 1) {
    return(identical(right, left))
  }

  # Are any cyclic rotations of `right` identical to left?
  any(sapply(seq_len(n), function(k) {
    identical(left, c(right[(k + 1):n], right[seq_len(k)]))
  }))
}
