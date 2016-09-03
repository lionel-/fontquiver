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
  check_font_family(variant, style, font_dejavu_files)
  check_font_ext(ext)

  base <- font_dejavu_files[[variant]][[style]]
  file <- font_file("dejavu", base, ext)
  version <- font_version("dejavu")

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
    book = "ttf/DejaVuSans",
    bold = "ttf/DejaVuSans-Bold",
    oblique = "ttf/DejaVuSans-Oblique",
    `bold-oblique` = "ttf/DejaVuSans-BoldOblique",
    `extra-light` = "ttf/DejaVuSans-ExtraLight"
  ),
  `sans-condensed` = c(
    book = "ttf/DejaVuSansCondensed",
    bold = "ttf/DejaVuSansCondensed-Bold",
    oblique = "ttf/DejaVuSansCondensed-Oblique",
    `bold-oblique` = "ttf/DejaVuSansCondensed-BoldOblique"
  ),
  `sans-mono` = c(
    book = "ttf/DejaVuSansMono",
    bold = "ttf/DejaVuSansMono-Bold",
    oblique = "ttf/DejaVuSansMono-Oblique",
    `bold-oblique` = "ttf/DejaVuSansMono-BoldOblique"
  ),
  serif = c(
    book = "ttf/DejaVuSerif",
    bold = "ttf/DejaVuSerif-Bold",
    italic = "ttf/DejaVuSerif-Italic",
    `bold-italic` = "ttf/DejaVuSerif-BoldItalic"
  ),
  `serif-condensed` = c(
    book = "ttf/DejaVuSerifCondensed",
    bold = "ttf/DejaVuSerifCondensed-Bold",
    italic = "ttf/DejaVuSerifCondensed-Italic",
    `bold-italic` = "ttf/DejaVuSerifCondensed-BoldItalic"
  ),
  `math-tex-gyre` = c(
    regular = "ttf/DejaVuMathTexGyre"
  )
)
