# TODO: document
throw_data <- new_generic("throw_data", "siteswap")

# TODO: useful documentation
# TODO: add `n_cycles` arg here
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
