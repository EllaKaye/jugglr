.onLoad <- function(...) {
  S7::methods_register()
}

is_even <- function(x) {
  x %% 2 == 0
}

is_odd <- function(x) {
  x %% 2 == 1
}

is_whole_number <- function(x) {
  if (!(rlang::is_double(x, 1) || rlang::is_integer(x, 1))) {
    cli::cli_abort("x must be numeric, length 1.")
  }
  x %% 1 == 0
}

get_throws <- function(notation) {
  if (!(rlang::is_character(notation, 1))) {
    cli::cli_abort("notation must be a string")
  }
  throws_chr <- strsplit(notation, "") |>
    unlist()

  match(tolower(throws_chr), c(0:9, letters)) - 1
}

can_throw <- function(throws) {
  n <- length(throws)
  lands <- ((seq_len(n) - 1) + throws) %% n
  anyDuplicated(lands) == 0
}

expand_siteswap <- function(pattern) {
  while (stringr::str_detect(pattern, "\\*")) {
    # Find position of first *
    star_pos <- stringr::str_locate(pattern, "\\*")[1, "start"]

    # Split at the *
    before_star <- stringr::str_sub(pattern, 1, star_pos - 1)
    after_star <- stringr::str_sub(pattern, star_pos + 1, -1)

    # Find the last parentheses group before *
    last_group <- stringr::str_extract(before_star, "\\([^)]+\\)[^(]*$")

    if (is.na(last_group)) {
      break # No group to mirror
    }

    # Extract just the parentheses content
    group_match <- stringr::str_extract(last_group, "\\([^)]+\\)")

    # Mirror it
    content <- stringr::str_remove_all(group_match, "[()]")
    parts <- stringr::str_split(content, ",")[[1]]
    mirrored <- paste0("(", paste(rev(parts), collapse = ","), ")")

    # Replace * with mirrored group
    pattern <- paste0(before_star, mirrored, after_star)
  }

  pattern
}

# x is a synchronous siteswap
extract_throws <- function(x) {
  stringr::str_extract_all(x, "\\w+")[[1]]
}

slide <- function(throws) {
  n <- length(throws)
  throws_no_x <- stringr::str_remove(throws, "x$")
  throws_num <- match(tolower(throws_no_x), c(1:9, letters))
  new <- numeric(n)

  for (i in seq_len(n)) {
    if (stringr::str_detect(throws[i], "x$")) {
      if (is_even(i)) {
        new[i] <- throws_num[i] - 1
      } else {
        new[i] <- throws_num[i] + 1
      }
    } else {
      new[i] <- throws_num[i]
    }
  }
  new
}
