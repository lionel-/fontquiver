#' Bitstream Vera fonts
#'
#' Available faces: \code{sans} (\code{roman}, \code{bold},
#' \code{oblique}, \code{oblique-bold}), \code{sans-mono}
#' (\code{roman}, \code{bold}, \code{oblique}, \code{bold-oblique}),
#' \code{serif} (\code{roman}, \code{bold}).
#'
#' @param variant Variant of the font, e.g. \code{"sans"} or \code{"serif"}.
#' @param style Style of the font, e.g. \code{"roman"} or \code{"bold"}.
#' @param ext One of \code{"ttf"}, \code{"svg"} or \code{"woff"}.
#' @return Path to the requested font. The version number of the font
#'   is given in the \code{version} attribute.
#' @references \url{https://www.gnome.org/fonts/}
#' @export
font_bitstream_vera <- function(variant = "sans", style = "roman",
                                ext = "ttf") {
  check_font_family(variant, style, bitstream_vera_files)
  check_font_ext(ext)

  base <- bitstream_vera_files[[variant]][[style]]
  file <- font_file("bitstream-vera", base, ext)
  version <- font_version("bitstream-vera")

  structure(class = "font", list(
    file = file,
    name = "Bitstream Vera",
    variant = variant,
    style = style,
    version = version
  ))
}

bitstream_vera_files <- list(
  sans = c(
    roman = "Vera",
    bold = "VeraBd",
    oblique = "VeraIt",
    `bold-oblique` = "VeraBI"
  ),
  `sans-mono` = c(
    roman = "VeraMono",
    bold = "VeraMoBd",
    oblique = "VeraMoIt",
    `bold-oblique` = "VeraMoBI"
  ),
  serif = c(
    roman = "VeraSe",
    bold = "VeraSeBd"
  )
)
