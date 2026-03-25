# TODO: sort this file.
# Maybe split into separate util_ files for different types of functions (e.g. plotting),
# or different classes.

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

get_throws <- function(sequence) {
  if (!(rlang::is_character(sequence, 1))) {
    cli::cli_abort("sequence must be a string")
  }
  throws_chr <- strsplit(sequence, "") |>
    unlist()

  match(tolower(throws_chr), c(0:9, letters)) - 1
}

# MAYBE: go back to calling this has_collisions,
# as that is the problem for vanilla and pure sync (non-multiplex) siteswap
can_throw <- function(throws) {
  n <- length(throws)
  lands <- ((seq_len(n) - 1) + throws) %% n
  anyDuplicated(lands) == 0
}

# x is a synchronous siteswap
extract_throws <- function(x) {
  stringr::str_extract_all(x, "\\w+")[[1]]
}

orbits <- function(siteswap) {
  p <- siteswap@period
  n_props <- siteswap@n_props

  max_len <- p * n_props
}


# To turn synchronous notation with `*` into its full version
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

# For use within the `animate` function
format_color <- function(color) {
  paste0("{", paste(color, collapse = ","), "}")
}

format_colors <- function(colors) {
  colors |>
    col2rgb() |>
    apply(2, format_color) |>
    paste(collapse = "")
}

# TODO: helper functions for checking each named arg to `animate`, e.g. colors_string,
# that checks the input (e.g. with rlang::is_character) and returns a string "arg_name=checked_value"

colors_string <- function(colors) {
  if (identical(colors, "mixed")) {
    return("colors=mixed")
  }
  if (identical(colors, "orbits")) {
    return("colors=orbits")
  }

  colors_fmt = format_colors(colors)

  paste0("colors=", colors_fmt)
}

# for arg = bps, width, height, fps, slowdown
fmt_string <- function(arg, value) {
  if (length(value) != 1 || !is.numeric(value)) {
    message <- c(
      "{.var {arg}} must be a {.cls numeric} vector of length 1.",
      "i" = "It was class {.cls {class(value)}} of length {length(value)} instead."
    )

    cli::cli_abort(
      message,
      call = rlang::caller_env(),
      class = "jugglr_error_class_or_length"
    )
  }

  paste0(arg, "=", value)
}


# TODO: for animate, for dealing with args passed to ...
# Takes named list or vector, filters out NULL values
# collapses for GIF server url

# TODO: think about what args this needs, and how it wirks inside `animate`
# TODO: keep adding args as I add them to `animate` (or vice versa)
jugglinglab_url <- function(
  pattern,
  colors = NULL,
  bps = NULL,
  width = NULL,
  height = NULL,
  fps = NULL,
  slowdown = NULL,
  ...
) {
  url_segments <- paste0("pattern=", pattern)

  if (!is.null(colors)) {
    url_segments <- c(url_segments, colors_string(colors))
  }

  # give list of non-null args and their values
  numeric_params <- Filter(
    Negate(is.null),
    list(
      bps = bps,
      width = width,
      height = height,
      fps = fps,
      slowdown = slowdown
    )
  )

  url_segments <- c(
    url_segments,
    mapply(fmt_string, names(numeric_params), numeric_params)
  )

  # TODO: deal with ...

  paste0(
    "https://jugglinglab.org/anim?",
    paste(c(url_segments, "redirect=true"), collapse = ";")
  )
}

# TODO: test capturing args from ...

# Helper function to validate save path in `animate`
validate_path <- function(save, ext = "gif") {
  if (is.null(save)) {
    return(invisible(NULL))
  }

  if (!rlang::is_character(save, n = 1)) {
    cli::cli_abort(
      "`save` must be a single character string specifying a file path"
    )
  }

  if (tolower(tools::file_ext(save)) != tolower(ext)) {
    cli::cli_abort("`save` must specify a path ending in '.{ext}'")
  }

  parent_dir <- dirname(save)
  if (!dir.exists(parent_dir)) {
    cli::cli_abort("Directory does not exist: {.path {parent_dir}}")
  }

  if (file.access(parent_dir, mode = 2) != 0) {
    cli::cli_abort("Directory is not writable: {.path {parent_dir}}")
  }

  invisible(save)
}

# f2 <- function(arg1, ...) {
#   # f2 can now receive whatever arguments were passed via ...
#   dots <- list(...)
#   cat("arg1:", arg1, "\n")
#   cat("Received", length(dots), "additional arguments\n")
#   print(names(dots))
# }

# f1 <- function(arg1, ...) {
#   # Pass along whatever ... contains to f2
#   f2(arg1, ...)
# }

# # Example calls with different subsets of arguments
# f1("first", color = "red", size = 10)
# f1("second", alpha = 0.5, shape = "circle", width = 5)
