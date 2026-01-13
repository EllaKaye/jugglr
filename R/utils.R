.onLoad <- function(...) {
  S7::methods_register()
}

throw_data <- S7::new_generic("throw_data", "x")

timeline <- S7::new_generic("timeline", "x")

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

get_throws <- function(sequence) {
  if (!(rlang::is_character(sequence, 1))) {
    cli::cli_abort("sequence must be a string")
  }
  throws_chr <- strsplit(sequence, "") |>
    unlist()

  match(tolower(throws_chr), c(0:9, letters)) - 1
}

can_throw <- function(throws) {
  n <- length(throws)
  lands <- ((seq_len(n) - 1) + throws) %% n
  anyDuplicated(lands) == 0
}

# x is a synchronous siteswap
extract_throws <- function(x) {
  stringr::str_extract_all(x, "\\w+")[[1]]
}

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
  mirror_throws <- rev(sapply(throws, function(throw) {
    content <- str_remove_all(throw, "[()]")
    parts <- str_split(content, ",")[[1]]
    paste0("(", paste(rev(parts), collapse = ","), ")")
  }))

  # Combine original and mirror
  paste0(base_pattern, paste(mirror_throws, collapse = ""))
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

generate_parabola <- function(x1, x2, height, prop, beat, n_points = 100) {
  vx <- (x1 + x2) / 2 # vertex
  xs <- seq(x1, x2, length.out = n_points)
  ys <- height * (1 - ((xs - vx) / (vx - x1))^2)

  data.frame(x = xs, y = ys, prop = prop, beat = beat)
}
