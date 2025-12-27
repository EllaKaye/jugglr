is_whole_number <- function(x) {
  if (!(rlang::is_double(x, 1) || rlang::is_integer(x, 1))) {
    cli::cli_abort("x must be numeric, length 1.")
  }
  x %% 1 == 0
}
