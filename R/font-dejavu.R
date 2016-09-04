#' Dejavu fonts
#'
#' Available variants (style): \code{sans} (\code{book}, \code{bold},
#' \code{oblique}, \code{bold-oblique}, \code{extra-light}),
#' \code{sans-condensed} (\code{book}, \code{bold}, \code{oblique},
#' \code{bold-oblique}), \code{sans-mono} (\code{book}, \code{bold},
#' \code{oblique}, \code{bold-oblique}), \code{serif} (\code{book},
#' \code{bold}, \code{italic}, \code{bold-italic}),
#' \code{serif-condensed} (\code{book}, \code{bold}, \code{italic},
#' \code{bold-italic}), \code{math-tex-gyre} (\code{regular}).
#'
#' @inheritParams font_bitstream_vera
#' @return The path to the requested font is provided in the
#'   \code{file} field. The version number of the font is given in the
#'   \code{version} field.
#' @references \url{http://dejavu-fonts.org}
#' @export
font_dejavu <- function(variant = "sans", style = "book", ext = "ttf") {
  if (system.file("fonts", package = "fontHeavy") == "") {
    stop(call. = FALSE,
      "fontHeavy must be installed from Github. Please run:\n",
      "  install.packages('devtools')\n",
      "  devtools::install_github('lionel-/fontHeavy')"
    )
  }

  check_font_family(variant, style, font_dejavu_files)
  check_font_ext(ext)

  base <- font_dejavu_files[[variant]][[style]]
  file <- font_file("dejavu", base, ext, "fontHeavy")
  version <- font_version("dejavu", "fontHeavy")

  structure(class = "font", list(
    file = file,
    name = "DejaVu",
    variant = variant,
    style = style,
    version = version
  ))
}

font_dejavu_files <- list(
  sans = c(
    book = "DejaVuSans",
    bold = "DejaVuSans-Bold",
    oblique = "DejaVuSans-Oblique",
    `bold-oblique` = "DejaVuSans-BoldOblique",
    `extra-light` = "DejaVuSans-ExtraLight"
  ),
  `sans-condensed` = c(
    book = "DejaVuSansCondensed",
    bold = "DejaVuSansCondensed-Bold",
    oblique = "DejaVuSansCondensed-Oblique",
    `bold-oblique` = "DejaVuSansCondensed-BoldOblique"
  ),
  `sans-mono` = c(
    book = "DejaVuSansMono",
    bold = "DejaVuSansMono-Bold",
    oblique = "DejaVuSansMono-Oblique",
    `bold-oblique` = "DejaVuSansMono-BoldOblique"
  ),
  serif = c(
    book = "DejaVuSerif",
    bold = "DejaVuSerif-Bold",
    italic = "DejaVuSerif-Italic",
    `bold-italic` = "DejaVuSerif-BoldItalic"
  ),
  `serif-condensed` = c(
    book = "DejaVuSerifCondensed",
    bold = "DejaVuSerifCondensed-Bold",
    italic = "DejaVuSerifCondensed-Italic",
    `bold-italic` = "DejaVuSerifCondensed-BoldItalic"
  ),
  `math-tex-gyre` = c(
    regular = "DejaVuMathTexGyre"
  )
)

font_dejavu_files <- lapply(font_dejavu_files, function(variant) {
  vapply(variant, function(style) {
    file.path("ttf", style)
  }, character(1))
})
