#' @references \url{https://www.gnome.org/fonts/}
#' @import fontBitstreamVera
#' @include fontset.R
fontset_bitstream_vera <- function(variant = "sans", style = "roman") {
  font_get("Bitstream Vera", variant, style, pkg = "fontBitstreamVera")
}

fontset_bitstream_vera_files <- list(
  sans = list(
    roman = sans("Vera", face = "plain", weight = 80),
    oblique = sans("VeraIt", face = "italic", weight = 80),
    bold = sans("VeraBd", face = "bold", weight = 200),
    `bold-oblique` = sans("VeraBI", face = "bolditalic", weight = 200)
  ),
  `sans-mono` = list(
    roman = mono("VeraMono", face = "plain", weight = 80),
    oblique = mono("VeraMoIt", face = "italic", weight = 80),
    bold = mono("VeraMoBd", face = "bold", weight = 200),
    `bold-oblique` = mono("VeraMoBI", face = "bolditalic", weight = 200)
  ),
  serif = list(
    roman = serif("VeraSe", face = "plain", weight = 80),
    bold = serif("VeraSeBd", face = "bold", weight = 200)
  )
)

fontset_register("Bitstream Vera",
  getter = fontset_bitstream_vera,
  files = fontset_bitstream_vera_files
)
