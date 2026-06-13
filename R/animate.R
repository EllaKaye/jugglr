#' Animate a juggling pattern
#'
#' Generates an animated GIF of a juggling pattern via the
#' [JugglingLab GIF server](https://jugglinglab.org/html/animinfo.html) and
#' displays it in the RStudio viewer (or default browser). If `path` is given
#' the GIF is saved to disk instead.
#'
#' Note that setting `colors` can introduce a short delay while the server
#' renders the animation.
#'
#' @param pattern A siteswap pattern string (e.g. `"531"`) or any siteswap
#'   object: [vanillaSiteswap], [synchronousSiteswap], [multiplexSiteswap],
#'   [synchronousMultiplexSiteswap], or [passingSiteswap]. Passing patterns in
#'   p-notation (e.g. `"<3p 3|3p 3>"`) animate correctly; passing patterns in
#'   fractional notation (e.g. `"<4.5 3 3 | 3 4 3.5>"`) are not recognised by
#'   JugglingLab and cannot be animated.
#' @param colors Optional. A vector of R colours (one per prop), or one of the
#'   special strings `"mixed"` or `"orbits"`. Passed to JugglingLab.
#' @param prop Prop type: `"ball"`, `"ring"`, or `"image"`. If `NULL` (default)
#'   the JugglingLab default (a ball) is used.
#' @param bps Beats per second (numeric scalar). Controls the animation speed.
#' @param width,height Width and height of the animation in pixels (numeric
#'   scalars).
#' @param fps Frames per second (numeric scalar).
#' @param slowdown Slowdown factor (numeric scalar). The JugglingLab default is
#'   `2.0`; values greater than this slow the animation further, values less
#'   than `2.0` speed it up.
#' @param ... Additional named arguments passed to the JugglingLab GIF server.
#'   Pattern-setting arguments include `dwell`, `hands`, `body`, `propdiam`,
#'   `gravity`, `bouncefrac`, `squeezebeats`, `hss`, `handspec`,
#'   `dwellmax`, and `hold`. Animation arguments include `stereo`, `border`,
#'   `camangle`, `showground`, and `hidejugglers`. All values must be scalars.
#' @param path Path to save the GIF (must end in `.gif`). If `NULL` (default)
#'   the animation is displayed in the viewer.
#'
#' @returns Invisibly returns the path to the temporary HTML file (when
#'   displaying) or the save path (when `path` is given).
#'
#' @examples
#' \dontrun{
#' animate("531")
#' animate(vanillaSiteswap("531"), prop = "ring", bps = 5)
#' animate("531", path = tempfile(fileext = ".gif"))
#' }
#'
#' @export
animate <- function(
  pattern,
  colors = NULL,
  prop = NULL,
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
    prop,
    bps,
    width,
    height,
    fps,
    slowdown,
    ...
  )

  # If path is provided, download the GIF
  if (!is.null(path)) {
    utils::download.file(gif_url, destfile = path, mode = "wb", quiet = TRUE)
    cli::cli_alert_success("Animation saved to: {.path {path}}")
    return(invisible(path))
  }

  # Otherwise, show in viewer
  html <- sprintf(
    '<!DOCTYPE html>
<html>
<head>
  <style>
    body {
      margin: 0; padding: 0;
      display: flex;
      justify-content: center;
      align-items: center;
      min-height: 100vh;
      font-family: sans-serif;
      color: #555;
    }
    #container { position: relative; text-align: center; }
    #loader {
      position: absolute;
      top: 50%%; left: 50%%;
      transform: translate(-50%%, -50%%);
    }
    #animation { max-width: 100%%; height: auto; }
  </style>
</head>
<body>
  <div id="container">
    <div id="loader">Rendering animation, please wait\u2026</div>
    <img id="animation" src="%s" alt="Juggling animation"
         onload="document.getElementById(\'loader\').style.display=\'none\';"
         onerror="document.getElementById(\'loader\').textContent=\'Animation failed to load.\';" />
  </div>
</body>
</html>',
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


# Non-exported helpers ---------------------------------------------------

format_color <- function(color) {
  paste0("{", paste(color, collapse = ","), "}")
}

format_colors <- function(colors) {
  tryCatch(
    colors |>
      grDevices::col2rgb() |>
      apply(2, format_color) |>
      paste(collapse = ""),
    error = function(e) {
      cli::cli_abort(
        c(
          "{.arg colors} contains invalid colour value(s).",
          "i" = "Use R colour names, hex codes, or the special strings {.val mixed} or {.val orbits}."
        ),
        class = "jugglr_error_invalid_color",
        parent = e
      )
    }
  )
}

colors_string <- function(colors) {
  if (identical(colors, "mixed")) {
    return("colors=mixed")
  }
  if (identical(colors, "orbits")) {
    return("colors=orbits")
  }

  colors_fmt <- format_colors(colors)

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
  prop = NULL,
  bps = NULL,
  width = NULL,
  height = NULL,
  fps = NULL,
  slowdown = NULL,
  ...
) {
  if (S7::S7_inherits(pattern, Siteswap)) {
    pattern <- pattern@sequence
  } else if (!rlang::is_string(pattern)) {
    cli::cli_abort(
      "{.arg pattern} must be a single character string or a {.cls Siteswap} object.",
      class = "jugglr_error_invalid_pattern"
    )
  }

  url_segments <- paste0("pattern=", pattern)

  if (!is.null(colors)) {
    url_segments <- c(url_segments, colors_string(colors))
  }

  if (!is.null(prop)) {
    prop <- rlang::arg_match(prop, c("ball", "ring", "image"))
    url_segments <- c(url_segments, paste0("prop=", prop))
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

  pattern_setting_args <- c(
    "dwell",
    "hands",
    "body",
    "propdiam",
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
    "mousepause",
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
        "{length(invalid_args)} invalid argument{?s} passed to {.arg ...}:",
        "x" = "Unknown: {.arg {invalid_args}}",
        "i" = "See the {.href [JugglingLab GIF server documentation](https://jugglinglab.org/html/animinfo.html)} for allowed arguments."
      ),
      call = rlang::caller_env(),
      class = "jugglr_error_invalid_args"
    )
  }

  # throw error if any value passed to `...` isn't a scaler
  non_scalar <- names(Filter(\(v) length(v) != 1, dots))
  if (length(non_scalar) > 0) {
    cli::cli_abort(
      c(
        "All arguments passed to `...` must be scalar (length 1):",
        "x" = "{.arg {non_scalar}} {?is/are} not scalar",
        "i" = "Lengths: {.val {lengths(dots[non_scalar])}}"
      ),
      class = "jugglr_error_not_scalar"
    )
  }

  # signal a message if user passed any ignored_args to `...`
  passed_ignored <- intersect(names(dots), ignored_args)
  if (length(passed_ignored) > 0) {
    cli::cli_inform(c(
      "i" = "{length(passed_ignored)} argument{?s} {?is/are} not supported in this context and will be ignored:",
      " " = "{.arg {passed_ignored}}"
    ))
    dots <- dots[setdiff(names(dots), ignored_args)]
  }

  if (length(dots) > 0) {
    # "key=value" pairs for args to `...`
    pairs <- paste0(names(dots), "=", unlist(dots))
    url_segments <- c(url_segments, pairs)
  }

  # Percent-encode the value of each "key=value" segment so characters such as
  # spaces and < | > (used in passing notation) and (), [] (sync/multiplex) are
  # legal in the URL. The "=" and ";" separators are kept structural.
  url_segments <- vapply(url_segments, encode_segment_value, character(1))

  paste0(
    "https://jugglinglab.org/anim?",
    paste(c(url_segments, "redirect=true"), collapse = ";")
  )
}

# Encode the value half of a "key=value" segment, leaving "key=" intact.
encode_segment_value <- function(segment) {
  parts <- strsplit(segment, "=", fixed = TRUE)[[1]]
  key <- parts[1]
  value <- paste(parts[-1], collapse = "=")
  paste0(key, "=", utils::URLencode(value, reserved = TRUE))
}

# Helper function to validate save path in `animate`
validate_path <- function(path, ext = "gif") {
  if (is.null(path)) {
    return(invisible(NULL))
  }

  if (!rlang::is_character(path, n = 1)) {
    cli::cli_abort(
      "{.arg path} must be a single character string specifying a file path.",
      class = "jugglr_error_not_string"
    )
  }

  if (tolower(tools::file_ext(path)) != tolower(ext)) {
    cli::cli_abort(
      "{.arg path} must specify a path ending in {.val {paste0('.', ext)}}.",
      class = "jugglr_error_bad_extension"
    )
  }

  parent_dir <- dirname(path)
  if (!dir.exists(parent_dir)) {
    cli::cli_abort(
      "Directory does not exist: {.path {parent_dir}}",
      class = "jugglr_error_dir_not_found"
    )
  }

  if (file.access(parent_dir, mode = 2) != 0) {
    cli::cli_abort(
      "Directory is not writable: {.path {parent_dir}}",
      class = "jugglr_error_not_writable"
    )
  }

  invisible(path)
}
