#' Bitstream Vera fonts
#'
#' Available variants (style): \code{sans} (\code{roman}, \code{bold},
#' \code{oblique}, \code{oblique-bold}), \code{sans-mono}
#' (\code{roman}, \code{bold}, \code{oblique}, \code{bold-oblique}),
#' \code{serif} (\code{roman}, \code{bold}).
#'
#' @param variant Variant of the font, e.g. \code{"sans"} or \code{"serif"}.
#' @param style Style of the font, e.g. \code{"roman"} or \code{"bold"}.
#' @return The path to the requested font is provided in the
#'   \code{ttf} and \code{woff} fields. The version number of the font
#'   is given in the \code{version} field.
#' @references \url{https://www.gnome.org/fonts/}
#' @include utils.R
#' @export
font_bitstream_vera <- function(variant = "sans", style = "roman") {
  if (system.file("fonts", package = "fontBitstreamVera") == "") {
    stop("fontBitstreamVera must be installed", call. = FALSE)
  }
  font_get("Bitstream Vera", variant, style, pkg = "fontBitstreamVera")
}

font_bitstream_vera_files <- list(
  sans = list(
    roman = font_sans("Vera", face = "plain", weight = 80),
    oblique = font_sans("VeraIt", face = "italic", weight = 80),
    bold = font_sans("VeraBd", face = "bold", weight = 200),
    `bold-oblique` = font_sans("VeraBI", face = "bolditalic", weight = 200)
  ),
  `sans-mono` = list(
    roman = font_mono("VeraMono", face = "plain", weight = 80),
    oblique = font_mono("VeraMoIt", face = "italic", weight = 80),
    bold = font_mono("VeraMoBd", face = "bold", weight = 200),
    `bold-oblique` = font_mono("VeraMoBI", face = "bolditalic", weight = 200)
  ),
  serif = list(
    roman = font_serif("VeraSe", face = "plain", weight = 80),
    bold = font_serif("VeraSeBd", face = "bold", weight = 200)
  )
)

fontset_register("Bitstream Vera", font_bitstream_vera, font_bitstream_vera_files)
