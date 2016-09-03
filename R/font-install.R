#' Download and install a font
#'
#' Fonts that are too large to be stored on CRAN can be downloaded
#' manually. They are stored along other fonts in in the library path
#' under \code{fontquiver/fonts}.
#'
#' @param font Only \code{dejavu} is currently available for download.
font_install <- function(font = "dejavu") {
  if (check_font_exists(font)) {
    stop("font is already installed", call. = FALSE)
  }

  message("Do you want to install font ", font, "?")
  while (!(answer <- readline("y/n: ")) %in% c("y", "n")) {
    message("Please answer 'y' or 'n'")
  }

  if (answer == "y") {
    dir <- system.file("fonts", package = "fontquiver")
    system(paste("make --directory", dir, font))
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}
