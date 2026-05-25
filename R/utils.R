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
    cli::cli_abort(
      "x must be a numeric scalar.",
      class = "jugglr_error_not_numeric"
    )
  }
  x %% 1 == 0
}

chr_throws_to_num <- function(throws) {
  match(tolower(throws), c(0:9, letters)) - 1
}

get_throws <- function(sequence) {
  if (!(rlang::is_character(sequence, 1))) {
    cli::cli_abort(
      "{.arg sequence} must be a single character string.",
      class = "jugglr_error_not_string"
    )
  }
  throws_chr <- strsplit(sequence, "") |>
    unlist()

  chr_throws_to_num(throws_chr)
}

can_throw <- function(throws) {
  n <- length(throws)
  lands <- ((seq_len(n) - 1) + throws) %% n
  anyDuplicated(lands) == 0
}
