# CHECK: difference between length(colors) and n_props
# CHECK: What is `format_colors(4)` doing?!
# TODO: all other args the the API takes
# TODO: make sure this takes either Siteswap objects or strings
# MAYBE: different arg name than "pattern"
# TODO: document
# TODO: in documentation, note that setting colors can lead to a delay in showing the animation.
# TODO: Is it possible to show a message in the viewer while waiting for the animation to render?
# Or do I want only Siteswap?
#' @param pattern Siteswap pattern
#' @param colors Optional vector of colors
#' @param ... Additional arguments to JugglingLab GIT server
#' @param path Path to save GIF. If `NULL` (default) view in viewer
#' @export
animate <- function(
  pattern,
  colors = NULL,
  bps = NULL,
  width = NULL,
  height = NULL,
  fps = NULL,
  slowdown = NULL,
  ...,
  path = NULL
) {
  if (!is.null(path)) {
    validate_path(path, ext = "gif")
  }

  gif_url <- jugglinglab_url(
    pattern,
    colors,
    bps,
    width,
    height,
    fps,
    slowdown,
    ...
  )

  # If path is provided, download the GIF
  if (!is.null(path)) {
    download.file(gif_url, destfile = path, mode = "wb", quiet = TRUE)
    cli::cli_alert_success("Animation saved to: {.path {path}}")
    return(invisible(path))
  }

  # Otherwise, show in viewer
  html <- sprintf(
    '
    <!DOCTYPE html>
    <html>
    <head>
      <style>
        body { 
          margin: 0; 
          padding: 0; 
          display: flex;
          justify-content: center;
          align-items: center;
          min-height: 100vh;
        }
        img { max-width: 100%%; height: auto; }
      </style>
    </head>
    <body>
      <img src="%s" alt="Juggling animation"/>
    </body>
    </html>
  ',
    gif_url
  )

  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "juggling_animation.html")
  writeLines(html, temp_file)

  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewer(temp_file)
  } else {
    utils::browseURL(temp_file)
  }

  invisible(temp_file)
}

#' Embed juggling animation in R Markdown or Quarto
#'
#' @rdname animate
#' @return knitr graphics object for embedding
#' @export
animate_markdown <- function(pattern, path, colors = NULL, ...) {
  animate(pattern, colors = colors, path = path, ...)
  knitr::include_graphics(path)
}


# Non-exported helpers ---------------------------------------------------

format_color <- function(color) {
  paste0("{", paste(color, collapse = ","), "}")
}

format_colors <- function(colors) {
  colors |>
    col2rgb() |>
    apply(2, format_color) |>
    paste(collapse = "")
}

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

# used with arg = bps, width, height, fps, slowdown
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
  named_params <- Filter(
    Negate(is.null),
    list(
      bps = bps,
      width = width,
      height = height,
      fps = fps,
      slowdown = slowdown
    )
  )

  # "key=value" pairs for named args
  url_segments <- c(
    url_segments,
    mapply(fmt_string, names(named_params), named_params)
  )

  # TODO: Check for `allowed_args` (from jugglinglab gif server docs)  # From jugglinglab documentation, not including named args
  pattern_setting_args <- c(
    "dwell",
    "hands",
    "body",
    "propdiam",
    "prop",
    "gravity",
    "bouncefrac",
    "squeezebeats",
    "hss",
    "handspec",
    "dwellmax",
    "hold"
  )

  animation_args <- c(
    "stereo",
    "border",
    "camangle",
    "showground",
    "hidejugglers"
  )

  ignored_args <- c(
    "startpaused",
    "mousepaused",
    "catchsound",
    "bouncesound",
    "view"
  )

  allowed_args <- c(pattern_setting_args, animation_args, ignored_args)

  # get args and values passed to `...`
  dots <- rlang::dots_list(..., .named = TRUE)

  invalid_args <- setdiff(names(dots), allowed_args)

  # throw error if invalid argument names to `...`
  if (length(invalid_args) > 0) {
    cli::cli_abort(
      c(
        "{length(invalid_args)} invalid argument{?s} passed to {.fn jugglinglab_url}:",
        "x" = "Unknown: {.arg {invalid_args}}",
        #"i" = "Allowed arguments: {.arg {allowed_args}}"
        #TODO: note about referring to jugglinglab documentation
      )
    )
  }

  # throw error if any value passed to `...` isn't a scaler
  non_scalar <- names(Filter(\(v) length(v) != 1, dots))
  if (length(non_scalar) > 0) {
    cli::cli_abort(c(
      "All arguments passed to `...` must be scalar (length 1):",
      "x" = "{.arg {non_scalar}} {?is/are} not scalar",
      "i" = "Lengths: {.val {lengths(dots[non_scalar])}}"
    ))
  }

  # TODO: if user has passed any of the ignored_args to ..., signal a message

  if (length(dots) > 0) {
    # "key=value" pairs for args to `...`
    pairs <- paste0(names(dots), "=", unlist(dots))
    url_segments <- c(url_segments, pairs)
  }

  paste0(
    "https://jugglinglab.org/anim?",
    paste(c(url_segments, "redirect=true"), collapse = ";")
  )
}

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
