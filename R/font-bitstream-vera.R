#' Bitstream Vera fonts
#'
#' Available variants (style): \code{sans} (\code{roman}, \code{bold},
#' \code{oblique}, \code{oblique-bold}), \code{sans-mono}
#' (\code{roman}, \code{bold}, \code{oblique}, \code{bold-oblique}),
#' \code{serif} (\code{roman}, \code{bold}).
#'
#' @param variant Variant of the font, e.g. \code{"sans"} or \code{"serif"}.
#' @param style Style of the font, e.g. \code{"roman"} or \code{"bold"}.
#' @param ext Either \code{"ttf"} or \code{"woff"}.
#' @return The path to the requested font is provided in the
#'   \code{file} field. The version number of the font is given in the
#'   \code{version} field.
#' @references \url{https://www.gnome.org/fonts/}
#' @export
font_bitstream_vera <- function(variant = "sans", style = "roman",
                                ext = "ttf") {
  if (system.file("fonts", package = "fontBitstreamVera") == "") {
    stop("fontBitstreamVera must be installed", call. = FALSE)
  }
  check_font_family(variant, style, font_bitstream_vera_files)
  check_font_ext(ext)

  base <- font_bitstream_vera_files[[variant]][[style]]
  file <- font_file("bitstream-vera", base, ext, package = "fontBitstreamVera")
  version <- font_version("bitstream-vera", package = "fontBitstreamVera")

  structure(class = "font", list(
    file = file,
    name = "Bitstream Vera",
    variant = variant,
    style = style,
    version = version
  ))
}

font_bitstream_vera_files <- list(
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
