# TODO: mixed and orbit for colours
# CHECK: difference between length(colors) and n_props
# CHECK: What is `format_colors(4)` doing?!
# TODO: all other args the the API takes
# TODO: make sure this takes either Siteswap objects or strings
# TODO: can I split out creating the URL to a separate function?
# How would that work with handling args to ...
# MAYBE: different arg name than "pattern"
# TODO: document
# TODO: in documentation, note that setting colors can lead to a delay in showing the animation.
# Or do I want only Siteswap?
#' @param pattern Siteswap pattern
#' @param colors Optional vector of colors
#' @param ... Additional arguments to JugglingLab GIT server
#' @param path Path to save GIF. If `NULL` (default) view in viewer
#' @export
animate <- function(pattern, colors = NULL, ..., path = NULL) {
  if (!is.null(path)) {
    validate_path(path, ext = "gif")
  }

  # TODO: add more named args as I add them to animate and/or jugglinglab_url
  # TODO: test ... is working properly
  gif_url <- jugglinglab_url(pattern, colors, ...)

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
