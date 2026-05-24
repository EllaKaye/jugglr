.onLoad <- function(...) {
  methods_register()
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

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

chr_throws_to_num <- function(throws) {
  match(tolower(throws), c(0:9, letters)) - 1
}

# TODO: helper function for `match` line
# I'm using this trick several times, but slightly differently on each occassion
get_throws <- function(sequence) {
  if (!(rlang::is_character(sequence, 1))) {
    cli::cli_abort("sequence must be a string")
  }
  throws_chr <- strsplit(sequence, "") |>
    unlist()

  chr_throws_to_num(throws_chr)
}

# MAYBE: go back to calling this has_collisions,
# as that is the problem for vanilla and pure sync (non-multiplex) siteswap
can_throw <- function(throws) {
  n <- length(throws)
  lands <- ((seq_len(n) - 1) + throws) %% n
  anyDuplicated(lands) == 0
}
