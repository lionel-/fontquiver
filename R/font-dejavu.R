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
font_dejavu <- function(variant = "sans", style = "book") {
  if (system.file("fonts", package = "fontHeavy") == "") {
    stop(call. = FALSE,
      "fontHeavy must be installed from Github. Please run:\n",
      "  install.packages('devtools')\n",
      "  devtools::install_github('lionel-/fontHeavy')"
    )
  }
  font_get("DejaVu", variant, style, pkg = "fontHeavy")
}

font_dejavu_files <- font_files(font_dejavu,
  sans = list(
    book = font_sans("ttf/DejaVuSans", face = "plain", weight = 80),
    oblique = font_sans("ttf/DejaVuSans-Oblique", face = "italic", weight = 80),
    bold = font_sans("ttf/DejaVuSans-Bold", face = "bold", weight = 200),
    `bold-oblique` = font_sans("ttf/DejaVuSans-BoldOblique", face = "bolditalic", weight = 200),
    `extra-light` = font_sans("ttf/DejaVuSans-ExtraLight", weight = 40)
  ),
  `sans-condensed` = list(
    book = font_extra("ttf/DejaVuSansCondensed", face = "plain", weight = 80),
    oblique = font_extra("ttf/DejaVuSansCondensed-Oblique", face = "italic", weight = 80),
    bold = font_extra("ttf/DejaVuSansCondensed-Bold", face = "bold", weight = 200),
    `bold-oblique` = font_extra("ttf/DejaVuSansCondensed-BoldOblique", face = "bolditalic", weight = 200)
  ),
  `sans-mono` = list(
    book = font_mono("ttf/DejaVuSansMono", face = "plain", weight = 80),
    oblique = font_mono("ttf/DejaVuSansMono-Oblique", face = "italic", weight = 80),
    bold = font_mono("ttf/DejaVuSansMono-Bold", face = "bold", weight = 200),
    `bold-oblique` = font_mono("ttf/DejaVuSansMono-BoldOblique", face = "bolditalic", weight = 200)
  ),
  serif = list(
    book = font_serif("ttf/DejaVuSerif", face = "plain", weight = 80),
    italic = font_serif("ttf/DejaVuSerif-Italic", face = "italic", weight = 80),
    bold = font_serif("ttf/DejaVuSerif-Bold", face = "bold", weight = 200),
    `bold-italic` = font_serif("ttf/DejaVuSerif-BoldItalic", face = "bolditalic", weight = 200)
  ),
  `serif-condensed` = list(
    book = font_extra("ttf/DejaVuSerifCondensed", face = "plain", weight = 80),
    italic = font_extra("ttf/DejaVuSerifCondensed-Italic", face = "italic", weight = 80),
    bold = font_extra("ttf/DejaVuSerifCondensed-Bold", face = "bold", weight = 200),
    `bold-italic` = font_extra("ttf/DejaVuSerifCondensed-BoldItalic", face = "bolditalic", weight = 200)
  ),
  `math-tex-gyre` = list(
    regular = font_extra("ttf/DejaVuMathTexGyre", face = "plain", weight = 80)
  )
)

font_register("DejaVu", font_dejavu, font_dejavu_files)
