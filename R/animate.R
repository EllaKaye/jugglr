# TODO: color
# TODO: all other args the the API takes
# TODO: make sure this takes either Siteswap objects or strings
# Or do I want only Siteswap?
#' @export
animate <- function(pattern) {
  # The GIF URL is constructed by appending ;redirect=true
  gif_url <- paste0(
    "https://jugglinglab.org/anim?pattern=",
    pattern,
    ";redirect=true"
  )

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
