# TODO: sort this file.
# Maybe split into separate util_ files for different types of functions and classes (e.g. utils-plotting, utils-sync, utils-methods)

.onLoad <- function(...) {
  methods_register()
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

# TODO: add functions `is_vanilla_siteswap`, `is_sync_siteswap` etc
# that I can use in combination `Siteswap` to validate sequence and `siteswap`
# to determine the subclass

# TODO: document
throw_data <- new_generic("throw_data", "siteswap")

# TODO: useful documentation
# TODO: add `n_cycles` arg here
# MAYBE: does this need to be a generic? Possibly only useful for vanilla,
# in which case it could be a regular function
# see https://rconsortium.github.io/S7/articles/generics-methods.html
# see https://rconsortium.github.io/S7/articles/packages.html for more on documenting generics/methods
#' Timeline
#'
#' Plot the timeline
#' @param siteswap A siteswap object
#' @param ... Additional arguments passed to methods
#'
#' @export
timeline <- new_generic("timeline", "siteswap")

#' @export
ladder <- new_generic("ladder", "siteswap")

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

# MAYBE: Finish this, if using
orbits <- function(siteswap) {
  p <- siteswap@period
  n_props <- siteswap@n_props

  max_len <- p * n_props
}

generate_parabola <- function(x1, x2, height, prop, beat, n_points = 100) {
  vx <- (x1 + x2) / 2 # vertex
  xs <- seq(x1, x2, length.out = n_points)
  ys <- height * (1 - ((xs - vx) / (vx - x1))^2)

  data.frame(x = xs, y = ys, prop = prop, beat = beat)
}
