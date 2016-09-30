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
#' @include utils.R
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

font_dejavu_files <- font_files(font_dejavu,
  sans = list(
    book = font_sans("ttf/DejaVuSans"),
    oblique = font_sans("ttf/DejaVuSans-Oblique", italic = TRUE),
    bold = font_sans("ttf/DejaVuSans-Bold", bold = TRUE),
    `bold-oblique` = font_sans("ttf/DejaVuSans-BoldOblique", italic = TRUE, bold = TRUE),
    `extra-light` = font_sans("ttf/DejaVuSans-ExtraLight")
  ),
  `sans-condensed` = list(
    book = font_extra("ttf/DejaVuSansCondensed"),
    oblique = font_extra("ttf/DejaVuSansCondensed-Oblique", italic = TRUE),
    bold = font_extra("ttf/DejaVuSansCondensed-Bold", bold = TRUE),
    `bold-oblique` = font_extra("ttf/DejaVuSansCondensed-BoldOblique", italic = TRUE, bold = TRUE)
  ),
  `sans-mono` = list(
    book = font_mono("ttf/DejaVuSansMono"),
    oblique = font_mono("ttf/DejaVuSansMono-Oblique", italic = TRUE),
    bold = font_mono("ttf/DejaVuSansMono-Bold", bold = TRUE),
    `bold-oblique` = font_mono("ttf/DejaVuSansMono-BoldOblique", italic = TRUE, bold = TRUE)
  ),
  serif = list(
    book = font_serif("ttf/DejaVuSerif"),
    italic = font_serif("ttf/DejaVuSerif-Italic", italic = TRUE),
    bold = font_serif("ttf/DejaVuSerif-Bold", bold = TRUE),
    `bold-italic` = font_serif("ttf/DejaVuSerif-BoldItalic", italic = TRUE, bold = TRUE)
  ),
  `serif-condensed` = list(
    book = font_extra("ttf/DejaVuSerifCondensed"),
    italic = font_extra("ttf/DejaVuSerifCondensed-Italic", italic = TRUE),
    bold = font_extra("ttf/DejaVuSerifCondensed-Bold", bold = TRUE),
    `bold-italic` = font_extra("ttf/DejaVuSerifCondensed-BoldItalic", italic = TRUE, bold = TRUE)
  ),
  `math-tex-gyre` = list(
    regular = font_extra("ttf/DejaVuMathTexGyre")
  )
)
